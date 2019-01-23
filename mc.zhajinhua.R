
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

nsims <- 1000000
sim.min <- 0.5
sim.max <- 52.5
df <- data.frame(
  # 3 Dragon cards
  D1=round(runif(n=nsims, min=sim.min, max=sim.max)),
  D2=round(runif(n=nsims, min=sim.min, max=sim.max)),
  D3=round(runif(n=nsims, min=sim.min, max=sim.max)),
  # 3 Phoenix cards
  P1=round(runif(n=nsims, min=sim.min, max=sim.max)),
  P2=round(runif(n=nsims, min=sim.min, max=sim.max)),
  P3=round(runif(n=nsims, min=sim.min, max=sim.max))
                )

# Rank of the cards
df$D1.rank <- floor((df$D1-1)/4) + 1
df$D2.rank <- floor((df$D2-1)/4) + 1
df$D3.rank <- floor((df$D3-1)/4) + 1
df$P1.rank <- floor((df$P1-1)/4) + 1
df$P2.rank <- floor((df$P2-1)/4) + 1
df$P3.rank <- floor((df$P3-1)/4) + 1

# Suit of the cards
df$D1.suit <- df$D1 %% 4
df$D2.suit <- df$D2 %% 4
df$D3.suit <- df$D3 %% 4
df$P1.suit <- df$P1 %% 4
df$P2.suit <- df$P2 %% 4
df$P3.suit <- df$P3 %% 4

# Valid or not the row (no repeat of same rank & suit)
df$Valid <- ( df$D1 != df$D2 ) & ( df$D1 != df$D3 ) & ( df$D1 != df$P1 ) & ( df$D1 != df$P2 ) & ( df$D1 != df$P3 ) &
  ( df$D2 != df$D3 ) & ( df$D2 != df$P1 ) & ( df$D2 != df$P2 ) & ( df$D2 != df$P3 ) &
  ( df$D3 != df$P1 ) & ( df$D3 != df$P2 ) & ( df$D3 != df$P3 ) &
  ( df$P1 != df$P2 ) & ( df$P1 != df$P3 ) &
  ( df$P2 != df$P3 )

# Remove invalid lines
df <- df[df$Valid,]

#
# For convenience, get hi,mid,lo card. Flush, etc.
#
d1.rank <- df$D1.rank + (df$D1.rank==1)*13
d2.rank <- df$D2.rank + (df$D2.rank==1)*13
d3.rank <- df$D3.rank + (df$D3.rank==1)*13
p1.rank <- df$P1.rank + (df$P1.rank==1)*13
p2.rank <- df$P2.rank + (df$P2.rank==1)*13
p3.rank <- df$P3.rank + (df$P3.rank==1)*13

df$DrgC1 <- pmax(d1.rank, d2.rank, d3.rank)
df$DrgC2 <- 0
df$DrgC3 <- pmin(d1.rank, d2.rank, d3.rank)
df$DrgC2 <- (d1.rank+d2.rank+d3.rank) - (df$DrgC1+df$DrgC3)

df$PhnC1 <- pmax(p1.rank, p2.rank, p3.rank)
df$PhnC2 <- 0
df$PhnC3 <- pmin(p1.rank, p2.rank, p3.rank)
df$PhnC2 <- (p1.rank+p2.rank+p3.rank) - (df$PhnC1+df$PhnC3)

#
# Indicators (straight, flush, pair, ...)
#
df$Drg.Unique <- ( d1.rank!=d2.rank & d1.rank!=d3.rank & d2.rank!=d3.rank )
df$Phn.Unique <- ( p1.rank!=p2.rank & p1.rank!=p3.rank & p2.rank!=p3.rank )

df$Drg.Straight <-
  ( df$Drg.Unique & (df$DrgC1-df$DrgC3==2) ) |
  ( df$DrgC1==14 & df$DrgC2==3 & df$DrgC3==2 )
df$Phn.Straight <-
  ( df$Phn.Unique & ( df$PhnC1-df$PhnC3==2 ) ) |
  ( df$PhnC1==14 & df$PhnC2==3 & df$PhnC3==2 )

df$Drg.Triple <- d1.rank==d2.rank & d2.rank==d3.rank
df$Phn.Triple <- p1.rank==p2.rank & p2.rank==p3.rank

df$Drg.Pair <- !df$Drg.Unique & !df$Drg.Triple
df$Phn.Pair <- !df$Phn.Unique & !df$Phn.Triple
df$Drg.Pair9toAce <- !df$Drg.Unique & !df$Drg.Triple & (df$DrgC2>=9)
df$Phn.Pair9toAce <- !df$Phn.Unique & !df$Phn.Triple & (df$PhnC2>=9)

df$Drg.Flush <- df$D1.suit==df$D2.suit & df$D2.suit==df$D3.suit
df$Phn.Flush <- df$P1.suit==df$P2.suit & df$P2.suit==df$P3.suit

# When Dragon or Phoenix has 2-3-5 cards
df$P235 <- df$PhnC1==5 & df$PhnC2==3 & df$PhnC3==2
df$D235 <- df$DrgC1==5 & df$DrgC2==3 & df$DrgC3==2

# 3 of a kind, including the "score"
df$D.3OAK <- df$Drg.Triple * df$DrgC1
df$P.3OAK <- df$Phn.Triple * df$PhnC1

#
# Straight Flush, including score
#
df$D.SF <- ( df$Drg.Straight & df$Drg.Flush ) * df$DrgC1
df$P.SF <- ( df$Phn.Straight & df$Phn.Flush ) * df$PhnC1

#
# Flush including score
# If the flushes are equal at 2nd/lo card, this will be handled by the score
#
df$D.FL <- ( !df$Drg.Straight & df$Drg.Flush ) * df$DrgC1
df$P.FL <- ( !df$Phn.Straight & df$Phn.Flush ) * df$PhnC1

#
# Straight
#
# 123 is the smallest straight
tmp123 <- df$DrgC1==14 & df$DrgC2==3 & df$DrgC3==2
df$D.Straight <- ( df$Drg.Straight & !df$Drg.Flush ) * ( (!tmp123)*df$DrgC1  + (tmp123==TRUE)*3 )
# 123 is the smallest straight
tmp123 <- df$PhnC1==14 & df$PhnC2==3 & df$PhnC3==2
df$P.Straight <- ( df$Phn.Straight & !df$Phn.Flush ) * ( (!tmp123)*df$PhnC1  + (tmp123==TRUE)*3 )

#
# Pair
#
df$D.Pair <- ( df$Drg.Pair==TRUE ) * df$DrgC2
df$P.Pair <- ( df$Phn.Pair==TRUE ) * df$PhnC2
df$D.Pair9toAce <- ( df$Drg.Pair==TRUE ) * ( df$DrgC2>=9 ) * df$DrgC2
df$P.Pair9toAce <- ( df$Phn.Pair==TRUE ) * ( df$PhnC2>=9 ) * df$PhnC2

#
# Final Score of Dragon/Phoenix
#
df$D.Score <- (15**7)*df$D.3OAK + (15**6)*df$D.SF + (15**5)*df$D.FL + (15**4)*df$D.Straight + (15**3)*df$D.Pair +
  (15**2)*df$DrgC1 + (15**1)*df$DrgC2 + (15**0)*df$DrgC3
df$P.Score <- (15**7)*df$P.3OAK + (15**6)*df$P.SF + (15**5)*df$P.FL + (15**4)*df$P.Straight + (15**3)*df$P.Pair +
  (15**2)*df$PhnC1 + (15**1)*df$PhnC2 + (15**0)*df$PhnC3

df$Winner <- (df$D.Score > df$P.Score)*2 + (df$P.Score > df$D.Score)*1

df$Winner.3OAK <- ( df$Winner==2 & df$D.3OAK>0 ) | ( df$Winner==1 & df$P.3OAK>0 )
df$Winner.SF   <- ( df$Winner==2 & df$D.SF>0 ) | ( df$Winner==1 & df$P.SF>0 )
df$Winner.FL   <- ( df$Winner==2 & df$D.FL>0 ) | ( df$Winner==1 & df$P.FL>0 )
df$Winner.Straight <- ( df$Winner==2 & df$D.Straight>0 ) | ( df$Winner==1 & df$P.Straight>0 )
df$Winner.Pair <- ( df$Winner==2 & df$D.Pair>0 ) | ( df$Winner==1 & df$P.Pair>0 )
df$Winner.Pair9toAce <- ( df$Winner==2 & df$D.Pair9toAce >0 ) | ( df$Winner==1 & df$P.Pair9toAce>0 )
df$Tie <- df$D.Score == df$P.Score
df$Winner.HiCard <- !df$Tie & !df$Winner.3OAK & !df$Winner.SF & !df$Winner.FL & !df$Winner.Straight & !df$Winner.Pair

# Pair Plus 8 (from Pair 9 upwards)
min.score.9pair <- (15**3)*9 + (15**2)*9 + (15**1)*9 + (15**0)*2
df$PairPlus8 <- df$Winner.Pair9toAce | df$Winner.Straight | df$Winner.FL | df$Winner.SF | df$Winner.3OAK

#
# Summaries
#
totalsims <- dim(df)[1]
print(paste("Total Simulations = ",totalsims),sep="")

prob.winner.dragon <- sum( (df$Winner==2)*1 ) / totalsims
prob.winner.dragon
prob.winner.phoenix <- sum( (df$Winner==1)*1 ) / totalsims
prob.winner.phoenix

prob.winner.3oak <- sum( (df$Winner.3OAK) * 1 ) / totalsims
prob.winner.3oak

prob.winner.sf <- sum( (df$Winner.SF) * 1 ) / totalsims
prob.winner.sf

prob.winner.flush <- sum( (df$Winner.FL) * 1 ) / totalsims
prob.winner.flush

prob.winner.straight <- sum( (df$Winner.Straight) * 1 ) / totalsims
prob.winner.straight

prob.winner.pair <- sum( (df$Winner.Pair) * 1 ) / totalsims
prob.winner.pair

prob.winner.pair9toAce <- sum( (df$Winner.Pair9toAce) * 1 ) / totalsims
prob.winner.pair9toAce

prob.winner.hicard <- sum( (df$Winner.HiCard) * 1 ) / totalsims
prob.winner.hicard

prob.tie <- sum( (df$Tie) * 1 ) / totalsims
prob.tie

prob.pairplus8 <- sum( (df$PairPlus8) * 1 ) / totalsims
prob.pairplus8


