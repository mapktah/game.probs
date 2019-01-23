
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

n.decks = 8

#
# Alternative way to calculate Baccarat frequencies, SUPER FAST!!
#
get.combination.count.baccarat.tablemethod <- function(n.decks)
{
  # Form a table of 6 columns (cards), ignore J-K, lump them as a 10-rank card
  v <- get.all.card.permutations(ncards=6, nranks=10, ndecks=n.decks)

  #
  # Sum of Card.1 & Card.2 = Player Score
  #
  v$PlayerScoreRaw <- (v$Card.1 + v$Card.2)%%10
  v$BankerScoreRaw <- (v$Card.3 + v$Card.4)%%10
  
  v$Natural <- v$Valid & ( v$PlayerScoreRaw>=8 | v$BankerScoreRaw>=8 )
  v$Player67 <- v$Valid & ( !v$Natural & (v$PlayerScoreRaw>=6 & v$PlayerScoreRaw<=7 ) )
  v$Player05 <- v$Valid & ( !v$Natural & (v$PlayerScoreRaw>=0 & v$PlayerScoreRaw<=5 ) )
  
  # Player picks 3rd card when Player score is 0-5
  v$Player3rdCard <- ( v$Player05 )
  
  # Banker picks 3rd card when Banker score is 0-5 on Player score 6-7, or when Player picks 3rd card with some conditions
  v$Banker3rdCard <- ( v$Player67 & v$BankerScoreRaw<=5 ) |
    ( v$Player3rdCard &
        (
          ( v$BankerScoreRaw<=2 ) |
          ( v$BankerScoreRaw==3 & v$Card.5!=8 ) |
          ( v$BankerScoreRaw==4 & v$Card.5>=2 & v$Card.5<=7 ) |
          ( v$BankerScoreRaw==5 & v$Card.5>=4 & v$Card.5<=7 ) | 
          ( v$BankerScoreRaw==6 & v$Card.5>=6 & v$Card.5<=7 )
        )
      )
  
  v$PlayerScoreFinal <- ( v$PlayerScoreRaw + v$Player3rdCard * v$Card.5 ) %% 10
  v$BankerScoreFinal <- ( v$BankerScoreRaw + v$Banker3rdCard * v$Card.6 ) %% 10
  
  v$Banker.Win <- v$Combinations * (v$BankerScoreFinal > v$PlayerScoreFinal)
  v$Player.Win <- v$Combinations * (v$BankerScoreFinal < v$PlayerScoreFinal)
  v$Tie        <- v$Combinations * (v$BankerScoreFinal == v$PlayerScoreFinal)
  
  # Banker/Player Pair Same Value (this is different from the traditional Banker/Player Pair
  # bets requiring same Rank)
  v$Banker.VPair <- v$Combinations * ( v$Card.3 == v$Card.4 )
  v$Player.VPair <- v$Combinations * ( v$Card.1 == v$Card.2 )
  
  # When Player doesn't draw & Banker draws, and Banker wins after draw overtaking a loss
  v$Banker.Overtake <- v$Combinations * ( ( !v$Player3rdCard & v$Banker3rdCard ) &
                          (v$BankerScoreRaw < v$PlayerScoreFinal) & (v$BankerScoreFinal > v$PlayerScoreFinal) )
  # When both have 3rd cards, Banker "overtakes" Player after Banker 3rd card
  v$Banker.Heng <- v$Combinations * ( ( v$Player3rdCard & v$Banker3rdCard ) &
                          (v$BankerScoreRaw < v$PlayerScoreFinal) & (v$BankerScoreFinal > v$PlayerScoreFinal) )
  # When both have 3rd cards, Player "overtakes" Banker after Banker 3rd card
  v$Player.Heng <- v$Combinations * ( ( v$Player3rdCard & v$Banker3rdCard ) &
                          (v$BankerScoreRaw > v$PlayerScoreFinal) & (v$BankerScoreFinal < v$PlayerScoreFinal) )
  
  return(v)
}

#
# Dragon Bonus
#
get.baccarat.dragonbonus.stats <- function(baccarat.table)
{
  v <- baccarat.table
  
  # To calculate Dragon Bonus, we split out some columns ()
  dragonb <- v[, c("Valid", "Combinations", "BankerScoreRaw", "PlayerScoreRaw", "BankerScoreFinal", "PlayerScoreFinal")]
  dragonb$ScoreDifRaw <- v$BankerScoreRaw - v$PlayerScoreRaw
  dragonb$ScoreDifFinal <- v$BankerScoreFinal - v$PlayerScoreFinal
  # Natural Tie
  dragonb$BankerNaturalTie <- (v$Valid & v$BankerScoreRaw>=8 & dragonb$ScoreDifFinal==0) * v$Combinations
  dragonb$PlayerNaturalTie <- (v$Valid & v$PlayerScoreRaw>=8 & dragonb$ScoreDifFinal==0) * v$Combinations
  # Natural Win
  dragonb$BankerNaturalWin <- (v$Valid & v$BankerScoreRaw>=8 & dragonb$ScoreDifFinal>0) * v$Combinations
  dragonb$PlayerNaturalWin <- (v$Valid & v$PlayerScoreRaw>=8 & dragonb$ScoreDifFinal<0) * v$Combinations
  
  # Record stats
  db.banker <- data.frame(BankerNaturalWin=sum(dragonb$BankerNaturalWin), BankerNaturalTie=sum(dragonb$BankerNaturalTie))
  db.player <- data.frame(PlayerNaturalWin=sum(dragonb$PlayerNaturalWin), PlayerNaturalTie=sum(dragonb$PlayerNaturalTie))
  # All wins by 4-9 margins must be unnatural
  for(i in c(9:4)) {
    cname <- paste("Banker.Win.",i,sep="")
    dragonb[,cname] <- (v$Valid & (v$Banker3rdCard | v$Player3rdCard) & dragonb$ScoreDifFinal==i) * v$Combinations
    db.banker[,cname] <- sum(dragonb[,cname])
    
    cname <- paste("Player.Win.",i,sep="")
    dragonb[,cname] <- (v$Valid & (v$Banker3rdCard | v$Player3rdCard) & dragonb$ScoreDifFinal==-i) * v$Combinations
    db.player[,cname] <- sum(dragonb[,cname])
  }
  # Loss Case
  db.banker$Loss <- totalcombs - sum(db.banker[1,])
  db.player$Loss <- totalcombs - sum(db.player[1,])
  
  # Sanity check
  calcombs.banker <- sum(db.banker[1,])
  calcombs.player <- sum(db.player[1,])
  if( (totalcombs != calcombs.banker) | (calcombs.player != calcombs.banker) )
    stop(paste("Error in Dragon Bonus, total combinations ",totalcombs," not equal to calculated ",calcombs,sep=""))
  
  # Second line as probability
  db.banker[2,] <- db.banker[1,]/totalcombs
  db.player[2,] <- db.player[1,]/totalcombs
  
  # 3rd line as market (decimal) odds
  db.banker[3,] <- c(2,1,31,11,7,5,3,2,0)
  db.player[3,] <- c(2,1,31,11,7,5,3,2,0)
  
  # Margins
  margin.db.banker <- 1 - sum( db.banker[2,]*db.banker[3,] )
  margin.db.player <- 1 - sum( db.player[2,]*db.player[3,] )
  
  list.sum.db <- list(BankerDragonBonus=db.banker, BankerDragonBonusMargin=margin.db.banker,
                      PlayerDragonBonus=db.player, PlayerDragonBonusMargin=margin.db.player)
  
  return(list.sum.db)
}

#summarize.combination.count.baccarat <- function(n.decks)
#{
v <- get.combination.count.baccarat.tablemethod(n.decks)
totalcombs <- sum(v$Combinations[v$Valid==TRUE])

# Get Dragon Bonus stats
get.baccarat.dragonbonus.stats(baccarat.table = v)

#
# Stat Summary
#
# Probability of natural case
p.natural <- sum(v$Combinations[v$Valid & v$Natural]) / totalcombs
p.natural.banker <- sum(v$Combinations[v$Valid & v$Natural & v$BankerScoreFinal>=8 & v$PlayerScoreFinal<8]) / totalcombs
p.natural.player <- sum(v$Combinations[v$Valid & v$Natural & v$PlayerScoreFinal>=8 & v$BankerScoreFinal<8]) / totalcombs
p.natural.both <- sum(v$Combinations[v$Valid & v$Natural & v$BankerScoreFinal>=8 & v$PlayerScoreFinal>=8]) / totalcombs
p.natural.tie <- sum(v$Combinations[v$Valid & v$Natural & v$BankerScoreFinal>=8 & v$PlayerScoreFinal==v$BankerScoreFinal]) / totalcombs
p.natural.both.bankerwin <- sum(v$Combinations[v$Valid & v$Natural & v$BankerScoreFinal>=8 & v$PlayerScoreFinal>=8 & v$BankerScoreFinal>v$PlayerScoreFinal]) / totalcombs
p.natural.both.playerwin <- sum(v$Combinations[v$Valid & v$Natural & v$BankerScoreFinal>=8 & v$PlayerScoreFinal>=8 & v$BankerScoreFinal<v$PlayerScoreFinal]) / totalcombs

#
# Non-Natural: Player 6-7 Case
#
p.notnatural <- sum(v$Combinations[v$Valid & !v$Natural]) / totalcombs
p.notnatural.player67 <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6]) / totalcombs
p.notnatural.player67.banker67 <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw>=6]) / totalcombs
p.notnatural.player67.banker67.tie <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw==v$PlayerScoreRaw]) / totalcombs
p.notnatural.player67.banker67.win <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw>v$PlayerScoreRaw]) / totalcombs

# Case banker 0-5 and draw 3rd card
p.notnatural.player67.banker05 <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw<=5]) / totalcombs
p.notnatural.player67.banker05.win <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw<=5 & v$BankerScoreFinal>v$PlayerScoreFinal]) / totalcombs
p.notnatural.player67.banker05.tie <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw<=5 & v$BankerScoreFinal==v$PlayerScoreFinal]) / totalcombs
p.notnatural.player67.banker05.lose <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw>=6 & v$BankerScoreRaw<=5 & v$BankerScoreFinal<v$PlayerScoreFinal]) / totalcombs

#
# Player 0-5
#
p.player05 <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5]) / totalcombs
# After 3rd card, Player is ahead of Banker's first 2 cards
p.player05.3rdcard.ahead <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal>v$BankerScoreRaw]) / totalcombs
p.player05.3rdcard.ahead.bkdraw <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal>v$BankerScoreRaw & v$Banker3rdCard]) / totalcombs
p.player05.3rdcard.ahead.bkdraw.bkwin <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal>v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal>v$PlayerScoreFinal]) / totalcombs
p.player05.3rdcard.ahead.bkdraw.tie <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal>v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal==v$PlayerScoreFinal]) / totalcombs
p.player05.3rdcard.ahead.bkdraw.plwin <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal>v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal<v$PlayerScoreFinal]) / totalcombs
# Banker do not draw 3rd card, JBL stand
p.player05.3rdcard.ahead.bkstand <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal>v$BankerScoreRaw & !v$Banker3rdCard]) / totalcombs

# After 3rd card, Player ties with Banker's first 2 cards
p.player05.3rdcard.tie <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==v$BankerScoreRaw]) / totalcombs
p.player05.3rdcard.tie.bkdraw <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==v$BankerScoreRaw & v$Banker3rdCard]) / totalcombs
p.player05.3rdcard.tie.bkdraw.bkwin <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal>v$PlayerScoreFinal]) / totalcombs
p.player05.3rdcard.tie.bkdraw.tie <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal==v$PlayerScoreFinal]) / totalcombs
p.player05.3rdcard.tie.bkdraw.plwin <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal<v$PlayerScoreFinal]) / totalcombs
# Banker do not draw 3rd card, stand with Tie
p.player05.3rdcard.tie.bkstand <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==v$BankerScoreRaw & !v$Banker3rdCard]) / totalcombs

# After 3rd card, Player is behind Banker's first 2 cards
p.player05.3rdcard.behind <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal<v$BankerScoreRaw]) / totalcombs
p.player05.3rdcard.behind.bkdraw <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal<v$BankerScoreRaw & v$Banker3rdCard]) / totalcombs
p.player05.3rdcard.behind.bkdraw.bkwin <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal<v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal>v$PlayerScoreFinal]) / totalcombs
p.player05.3rdcard.behind.bkdraw.tie <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal<v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal==v$PlayerScoreFinal]) / totalcombs
p.player05.3rdcard.behind.bkdraw.plwin <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal<v$BankerScoreRaw & v$Banker3rdCard & v$BankerScoreFinal<v$PlayerScoreFinal]) / totalcombs
# Banker do not draw 3rd card, stand with Win
p.player05.3rdcard.behind.bkstand <- sum(v$Combinations[v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal<v$BankerScoreRaw & !v$Banker3rdCard]) / totalcombs

#
# Detailed Cases of Player Moving Ahead
#
for(X in c(0:9))
{
  # Not natural case with player 2 card score <=5, player 3 card score > banker 2 card score, and banker draws 3rd card
  cond <- v$Valid & !v$Natural & v$PlayerScoreRaw<=5 & v$PlayerScoreFinal==X & v$PlayerScoreFinal>v$BankerScoreRaw & v$Banker3rdCard
  
  p.player05.3rdcard.X.ahead.bkdraw <- sum(v$Combinations[cond]) / totalcombs
  p.player05.3rdcard.X.ahead.bkdraw.win <- sum(v$Combinations[cond & v$BankerScoreFinal>v$PlayerScoreFinal]) / totalcombs
  p.player05.3rdcard.X.ahead.bkdraw.tie <- sum(v$Combinations[cond & v$BankerScoreFinal==v$PlayerScoreFinal]) / totalcombs
  p.player05.3rdcard.X.ahead.bkdraw.lose <- sum(v$Combinations[cond & v$BankerScoreFinal<v$PlayerScoreFinal]) / totalcombs
  
  print(paste("For Player Score of 3 cards = ",X,sep=""))
  print(paste("Total Player Ahead=",p.player05.3rdcard.X.ahead.bkdraw,", Banker Win=",p.player05.3rdcard.X.ahead.bkdraw.win,", Tie=",
              p.player05.3rdcard.X.ahead.bkdraw.tie,", Player Win=",p.player05.3rdcard.X.ahead.bkdraw.lose,sep=""))
  print(paste("Conditional Banker Win=",p.player05.3rdcard.X.ahead.bkdraw.win/p.player05.3rdcard.X.ahead.bkdraw,", Tie=",
              p.player05.3rdcard.X.ahead.bkdraw.tie/p.player05.3rdcard.X.ahead.bkdraw,
              ", Player Win=",p.player05.3rdcard.X.ahead.bkdraw.lose/p.player05.3rdcard.X.ahead.bkdraw,sep=""))
  print("")
}

# Banker Win
cond <- v$Valid & v$BankerScoreFinal>v$PlayerScoreFinal
p.bwin <- sum(v$Combinations[cond]) / totalcombs
p.bwin.natural <- sum(v$Combinations[cond & v$Natural]) / totalcombs
cond <- cond & !v$Natural
p.bwin.unnatural <- sum(v$Combinations[cond]) / totalcombs
p.bwin.unnatural.by9 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)==9]) / totalcombs
p.bwin.unnatural.by8 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)==8]) / totalcombs
p.bwin.unnatural.by7 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)==7]) / totalcombs
p.bwin.unnatural.by6 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)==6]) / totalcombs
p.bwin.unnatural.by5 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)==5]) / totalcombs
p.bwin.unnatural.by4 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)==4]) / totalcombs
p.bwin.unnatural.by0to3 <- sum(v$Combinations[cond & (v$BankerScoreFinal-v$PlayerScoreFinal)<4]) / totalcombs
# Super Six
cond <- v$Valid & v$BankerScoreFinal>v$PlayerScoreFinal
for(i in c(1:9)) {
  p.bwin.scoreX <- sum(v$Combinations[cond & v$BankerScoreFinal==i]) / totalcombs
  p.bwin.scoreX.natural <- sum(v$Combinations[cond & v$BankerScoreFinal==i & v$Natural]) / totalcombs
  p.bwin.scoreX.unnatural <- sum(v$Combinations[cond & v$BankerScoreFinal==i & !v$Natural]) / totalcombs
  print(paste("Banker Win on ",i,"=",p.bwin.scoreX,", (Natural=",p.bwin.scoreX.natural,", Unnatural=",p.bwin.scoreX.unnatural,")",sep=""))
}

# Player Win
cond <- v$Valid & v$PlayerScoreFinal>v$BankerScoreFinal
p.pwin <- sum(v$Combinations[cond]) / totalcombs
p.pwin.natural <- sum(v$Combinations[cond & v$Natural]) / totalcombs
cond <- cond & !v$Natural
p.pwin.unnatural <- sum(v$Combinations[cond]) / totalcombs
p.pwin.unnatural.by9 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)==9]) / totalcombs
p.pwin.unnatural.by8 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)==8]) / totalcombs
p.pwin.unnatural.by7 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)==7]) / totalcombs
p.pwin.unnatural.by6 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)==6]) / totalcombs
p.pwin.unnatural.by5 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)==5]) / totalcombs
p.pwin.unnatural.by4 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)==4]) / totalcombs
p.pwin.unnatural.by0to3 <- sum(v$Combinations[cond & (v$PlayerScoreFinal-v$BankerScoreFinal)<4]) / totalcombs

# Tie
cond <- v$Valid & v$PlayerScoreFinal==v$BankerScoreFinal
p.tie <- sum(v$Combinations[cond]) / totalcombs
p.tie.natural <- sum(v$Combinations[cond & v$Natural]) / totalcombs
p.tie.unnatural <- sum(v$Combinations[cond & !v$Natural]) / totalcombs

  bwin <- sum(v$Banker.Win[v$Valid])
  pwin <- sum(v$Player.Win[v$Valid])
  tie  <- sum(v$Tie[v$Valid])
  # Exotic
  b.natural <- sum(v$Valid *(v$BankerScoreRaw>=8) * v$Combinations) 
  p.natural <- sum(v$Valid * (v$PlayerScoreRaw>=8) * v$Combinations) 
  bp.natural <- sum(v$Valid * (v$BankerScoreRaw>=8 & v$PlayerScoreRaw>=8) * v$Combinations)
  # Both Draw 3rd Card
  b.draw <- sum(v$Valid * v$Banker3rdCard * v$Combinations)
  p.draw <- sum(v$Valid * v$Player3rdCard * v$Combinations)
  bp.draw <- sum(v$Valid * (v$Banker3rdCard * v$Player3rdCard) * v$Combinations)
  # 6-7
  # Value Pairs & Heng bets
  bvpair <- sum(v$Banker.VPair[v$Valid])
  pvpair <- sum(v$Player.VPair[v$Valid])
  bovertake <- sum(v$Banker.Overtake[v$Valid])
  bheng <- sum(v$Banker.Heng[v$Valid])
  pheng <- sum(v$Player.Heng[v$Valid])
  # Other Exotics
  # TODO: On average win by how many points, regardless of natural or not
  # TODO: How many times Banker makes the right decision to draw 3rd card (means before draw, Player winning)
  
  n <- bwin + pwin + tie
  
  bwin/n ; pwin/n ; tie/n
  # Exotics
  b.natural/n ; p.natural/n ; bp.natural/n
  b.draw/n ; p.draw/n ; bp.draw/n
  bvpair/n ; pvpair/n
  bovertake/n ; bheng/n ; pheng/n
  
#
# Other stats
#
  v$BankerPlayerFinalSum <- (v$BankerScoreFinal + v$PlayerScoreFinal) %% 10
  v$BankerPlayerFinalSumEvenOdd <- v$BankerPlayerFinalSum %% 2
  # Probability of even/odd final sum
  sum(v$Combinations[v$BankerPlayerFinalSumEvenOdd==0 & v$Valid]) / totalcombs
  sum(v$Combinations[v$BankerPlayerFinalSumEvenOdd==1 & v$Valid]) / totalcombs
  # Banker Dragon 7 (Banker 3-card total is 7 and wins)
  sum(v$Combinations[v$Valid & v$Banker3rdCard & v$BankerScoreFinal==7 & v$PlayerScoreFinal<7]) / totalcombs
  # Player Phoenix 8 (Player 3-card total is 8 and wins)
  sum(v$Combinations[v$Valid & v$Player3rdCard & v$PlayerScoreFinal==8 & v$BankerScoreFinal<8]) / totalcombs
  
  # Insurance
  # Probability Banker 9 and draw
  sum(v$Combinations[v$Valid & v$Natural & v$PlayerScoreFinal==9 & v$BankerScoreFinal==9]) / 
    sum(v$Combinations[v$Valid & v$Natural &v$BankerScoreFinal==9]) 

#
# Player/Banker Odd/Even
#
sum(v$Combinations[v$Valid & v$PlayerScoreFinal%%2==0]) / totalcombs
sum(v$Combinations[v$Valid & v$PlayerScoreFinal%%2==1]) / totalcombs
sum(v$Combinations[v$Valid & v$BankerScoreFinal%%2==0]) / totalcombs
sum(v$Combinations[v$Valid & v$BankerScoreFinal%%2==1]) / totalcombs

for(i in c(0:9)) {
  prb.tmp <- sum(v$Combinations[v$Valid & v$PlayerScoreFinal==i]) / totalcombs
  print( paste("Player Score ", i, ", Prob = ", prb.tmp, sep="") )
}

for(i in c(0:9)) {
  prb.tmp <- sum(v$Combinations[v$Valid & v$BankerScoreFinal==i]) / totalcombs
  print( paste("Banker Score ", i, ", Prob = ", prb.tmp, sep="") )
}

#
# SAG Over/Under Bets
#
sum(v$Combinations[v$Valid & v$PlayerScoreFinal+v$BankerScoreFinal<10]) / totalcombs

