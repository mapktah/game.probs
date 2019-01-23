
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

#
# Our method below by taking only one hand is not accurate because taking symmetries assumes
# that all combinations are valid. But since this is finite 1-deck, means that some combinations
# won't exist when we use symmetry, and the error can be huge.
#

NOBULL.SCORE <- 0
# Same as Jinniu (so means will be overwritten with Jinniu score)
FIVEFACE.SCORE <- 11
YINNIU.SCORE <- 12
# Same as 5 Face/Gong
JINNIU_5GONG.SCORE <- 13
ZHADAN.SCORE <- 14
WUSHIAONIU.SCORE <- 15
MAX.SCORE <- 15

#
# Bullbull / BullFight
#
get.combination.count.bullbull.tablemethod <- function(n.decks, game="BULLBULL")
{
  # Since for Bullbull, Player/Banker is symmetrical, we only need to generate 1 hand
  # Form a table of 5 columns (cards), ignore J-K, lump them as a 10-rank card
  ncards <- 5
  nranks <- 13
  v <- get.all.card.permutations(ncards=5, nranks=13, ndecks=n.decks)
  
  # Remove invalid combinations
  v <- v[v$Valid,]
  
  # 1-9 is just 1-9 as value, 10,J,Q,K (card number 0) is 10
  v$Card.1.Value <- v$Card.1 * (v$Card.1<10) + 10 * (v$Card.1>=10 | v$Card.1==0)
  v$Card.2.Value <- v$Card.2 * (v$Card.2<10) + 10 * (v$Card.2>=10 | v$Card.2==0)
  v$Card.3.Value <- v$Card.3 * (v$Card.3<10) + 10 * (v$Card.3>=10 | v$Card.3==0)
  v$Card.4.Value <- v$Card.4 * (v$Card.4<10) + 10 * (v$Card.4>=10 | v$Card.4==0)
  v$Card.5.Value <- v$Card.5 * (v$Card.5<10) + 10 * (v$Card.5>=10 | v$Card.5==0)
  
  #10 Combinations of 3 card
  NoBull.test <- rep(TRUE, dim(v)[1])
  for(i in c(1:3)) {
    for(j in c((i+1):4)) {
      for(k in c((j+1):5)) {
        print(paste(i, ",", j, ",", k, sep=""))
        # 1st, 2nd, 3rd Card
        ci <- v[,paste("Card.",i,".Value",sep="")]
        cj <- v[,paste("Card.",j,".Value",sep="")]
        ck <- v[,paste("Card.",k,".Value",sep="")]
        
        NoBull.test <- NoBull.test & ( ( ci + cj + ck ) %% 10 != 0 )
      }
    }
  }
  # NoBull <- ( (v$Card.3.Value + v$Card.4.Value + v$Card.5.Value)%%10 != 0 ) &   # 1,2
  #   ( (v$Card.2.Value + v$Card.4.Value + v$Card.5.Value)%%10 != 0 ) &   # 1,3
  #   ( (v$Card.2.Value + v$Card.3.Value + v$Card.5.Value)%%10 != 0 ) &   # 1,4
  #   ( (v$Card.2.Value + v$Card.3.Value + v$Card.4.Value)%%10 != 0 ) &   # 1,5
  #   ( (v$Card.1.Value + v$Card.4.Value + v$Card.5.Value)%%10 != 0 ) &   # 2,3
  #   ( (v$Card.1.Value + v$Card.3.Value + v$Card.5.Value)%%10 != 0 ) &   # 2,4
  #   ( (v$Card.1.Value + v$Card.3.Value + v$Card.4.Value)%%10 != 0 ) &   # 2,5
  #   ( (v$Card.1.Value + v$Card.2.Value + v$Card.5.Value)%%10 != 0 ) &   # 3,4
  #   ( (v$Card.1.Value + v$Card.2.Value + v$Card.4.Value)%%10 != 0 ) &   # 3,5
  #   ( (v$Card.1.Value + v$Card.2.Value + v$Card.3.Value)%%10 != 0 )     # 4,5
  # stopifnot(sum(1*(NoBull==NoBull.test))==dim(v)[1])
  NoBull <- NoBull.test
  
  # Bull Score
  # TODO Convert 0 score to 10 directly from here, otherwise will not be accurate!
  BullScore.test <- rep(0, dim(v)[1])
  for(i in c(1:3)) {
    for(j in c((i+1):4)) {
      for(k in c((j+1):5)) {
        print(paste(i, ",", j, ",", k, sep=""))
        # 1st, 2nd, 3rd Card
        ci <- v[,paste("Card.",i,".Value",sep="")]
        cj <- v[,paste("Card.",j,".Value",sep="")]
        ck <- v[,paste("Card.",k,".Value",sep="")]
        # 2 remaining cards
        index.remaining <- setdiff(c(1:5),c(i,j,k))
        cx <- v[,paste("Card.",index.remaining[1],".Value",sep="")]
        cy <- v[,paste("Card.",index.remaining[2],".Value",sep="")]
        BullScore.test <- pmax(BullScore.test, ( (cx + cy)%%10 + 10*(( cx + cy)%%10==0) ) * ( (ci + cj + ck)%%10 == 0 ))
      }
    }
  }
  # v$BullScore <- pmax(
  #   ( ( v$Card.1.Value + v$Card.2.Value)%%10 + 10*(( v$Card.1.Value + v$Card.2.Value)%%10==0) ) * ( (v$Card.3.Value + v$Card.4.Value + v$Card.5.Value)%%10 == 0 ),#1,2
  #   ( ( v$Card.1.Value + v$Card.3.Value)%%10 + 10*(( v$Card.1.Value + v$Card.3.Value)%%10==0) ) * ( (v$Card.2.Value + v$Card.4.Value + v$Card.5.Value)%%10 == 0 ),#1,3
  #   ( ( v$Card.1.Value + v$Card.4.Value)%%10 + 10*(( v$Card.1.Value + v$Card.4.Value)%%10==0) ) * ( (v$Card.2.Value + v$Card.3.Value + v$Card.5.Value)%%10 == 0 ),#1,4
  #   ( ( v$Card.1.Value + v$Card.5.Value)%%10 + 10*(( v$Card.1.Value + v$Card.5.Value)%%10==0) ) * ( (v$Card.2.Value + v$Card.3.Value + v$Card.4.Value)%%10 == 0 ),#1,5
  #   ( ( v$Card.2.Value + v$Card.3.Value)%%10 + 10*(( v$Card.2.Value + v$Card.3.Value)%%10==0) ) * ( (v$Card.1.Value + v$Card.4.Value + v$Card.5.Value)%%10 == 0 ),#2,3
  #   ( ( v$Card.2.Value + v$Card.4.Value)%%10 + 10*(( v$Card.2.Value + v$Card.4.Value)%%10==0) ) * ( (v$Card.1.Value + v$Card.3.Value + v$Card.5.Value)%%10 == 0 ),#2,4
  #   ( ( v$Card.2.Value + v$Card.5.Value)%%10 + 10*(( v$Card.2.Value + v$Card.5.Value)%%10==0) ) * ( (v$Card.1.Value + v$Card.3.Value + v$Card.4.Value)%%10 == 0 ),#2,5
  #   ( ( v$Card.3.Value + v$Card.4.Value)%%10 + 10*(( v$Card.3.Value + v$Card.4.Value)%%10==0) ) * ( (v$Card.1.Value + v$Card.2.Value + v$Card.5.Value)%%10 == 0 ),#3,4
  #   ( ( v$Card.3.Value + v$Card.5.Value)%%10 + 10*(( v$Card.3.Value + v$Card.5.Value)%%10==0) ) * ( (v$Card.1.Value + v$Card.2.Value + v$Card.4.Value)%%10 == 0 ),#3,5
  #   ( ( v$Card.4.Value + v$Card.5.Value)%%10 + 10*(( v$Card.4.Value + v$Card.5.Value)%%10==0) ) * ( (v$Card.1.Value + v$Card.2.Value + v$Card.3.Value)%%10 == 0 )#4,5
  # )
  # stopifnot(sum(1*(v$BullScore==BullScore.test))==dim(v)[1])
  v$BullScore <- BullScore.test
  
  v$BullBullScore <- v$BullScore
  v$BullFightScore <- v$BullScore

  v$BullBullScore[NoBull] <- NOBULL.SCORE
  v$BullFightScore[NoBull] <- NOBULL.SCORE
  
  # Bullbull score is 11 (same as JINNIU Score, so will be taken over later completely), NoBull score is 0
  FiveFace <- (v$Card.1>10 | v$Card.1==0) & (v$Card.2>10 | v$Card.2==0) &
    (v$Card.3>10 | v$Card.3==0) &(v$Card.4>10 | v$Card.4==0) & (v$Card.5>10 | v$Card.5==0)
  v$BullBullScore[FiveFace] <- FIVEFACE.SCORE
  # BullFight calls it Jinniu, so nothing to do here
  
  # For BullFight Rules
  yinniu <- (v$Card.1>=10 | v$Card.1==0) & (v$Card.2>=10 | v$Card.2==0) &
    (v$Card.3>=10 | v$Card.3==0) &(v$Card.4>=10 | v$Card.4==0) & (v$Card.5>=10 | v$Card.5==0) &
    (v$Card.1==10 | v$Card.2==10 | v$Card.3==10 | v$Card.4==10 | v$Card.5==10)
  v$BullFightScore[yinniu] <- YINNIU.SCORE
  
  jinniu <- FiveFace
  v$BullFightScore[jinniu] <- JINNIU_5GONG.SCORE
  
  # Four of a kind
  zhadan <- (v$Card.1==v$Card.2 & v$Card.1==v$Card.3 & v$Card.1==v$Card.4) |
    (v$Card.1==v$Card.2 & v$Card.1==v$Card.3 & v$Card.1==v$Card.5) |
    (v$Card.1==v$Card.2 & v$Card.1==v$Card.4 & v$Card.1==v$Card.5) |
    (v$Card.1==v$Card.3 & v$Card.1==v$Card.4 & v$Card.1==v$Card.5) |
    (v$Card.2==v$Card.3 & v$Card.2==v$Card.4 & v$Card.2==v$Card.5)
  v$BullFightScore[zhadan] <- ZHADAN.SCORE
  
  # Consist only of A,2,3,4, and sum of the 5 cards <= 10
  smallset <- c(1,2,3,4)
  wushiaoniu <- is.element(v$Card.1,smallset) & is.element(v$Card.2,smallset) &
    is.element(v$Card.3,smallset) & is.element(v$Card.4,smallset) & is.element(v$Card.5,smallset) &
    (v$Card.1 + v$Card.2 + v$Card.3 + v$Card.4 + v$Card.5 <= 10)
  v$BullFightScore[wushiaoniu] <- WUSHIAONIU.SCORE

  # Compress permutations into combinations. We can do this because this 5 card is just for 1 hand,
  # means order of the cards is unimportant.
  df.count <- get.card.frequencies(ncards = ncards, nranks = nranks, card.permutations = v[,1:5])
  v <- cbind(v, df.count)
  
  # Aggregate by combination count (amazing compression of data!! >50 times!!)
  agg.v <- data.frame()
  if(game == "BULLFIGHT")
    agg.v <- aggregate(x=data.frame("Combinations"=v$Combinations),
                     by=list(Freq.0 = v$Freq.0, Freq.1=v$Freq.1, Freq.2=v$Freq.2, Freq.3=v$Freq.3, Freq.4=v$Freq.4,
                             Freq.5=v$Freq.5, Freq.6=v$Freq.6, Freq.7=v$Freq.7, Freq.8=v$Freq.8, Freq.9=v$Freq.9,
                             Freq.10=v$Freq.10, Freq.11=v$Freq.11, Freq.12=v$Freq.12,
                             Valid     = v$Valid,
                             BullScore = v$BullScore,
                             BullFightScore = v$BullFightScore),
                     FUN="sum")
  else
    agg.v <- aggregate(x=data.frame("Combinations"=v$Combinations),
                       by=list(Freq.0 = v$Freq.0, Freq.1=v$Freq.1, Freq.2=v$Freq.2, Freq.3=v$Freq.3, Freq.4=v$Freq.4,
                               Freq.5=v$Freq.5, Freq.6=v$Freq.6, Freq.7=v$Freq.7, Freq.8=v$Freq.8, Freq.9=v$Freq.9,
                               Freq.10=v$Freq.10, Freq.11=v$Freq.11, Freq.12=v$Freq.12,
                               Valid     = v$Valid,
                               BullScore = v$BullScore,
                               BullBullScore = v$BullBullScore),
                       FUN="sum")
  
  return(agg.v)
}

n.decks <- 1
v.bb <- get.combination.count.bullbull.tablemethod(n.decks = n.decks, game="BULLBULL")
v.bf <- get.combination.count.bullbull.tablemethod(n.decks = n.decks, game="BULLFIGHT")


######################################################################################
# Bull Fight
######################################################################################

#
# BullFight: Calculate for Hand 2 win is good enough, due to symmetry
#
v <- v.bf
prob.bf.matrix <- matrix(data=0, nrow = MAX.SCORE+1, ncol = MAX.SCORE+1)
for(sc1 in c(0:MAX.SCORE))
{
  for(sc2 in c(sc1:MAX.SCORE))
  {
    # Don't do for (0,0), too many combinations, we will just estimate this one
    if(sc1==0 & sc2==0) next
    
    print(paste("Doing for Hand 1 Score=", sc1, ", Hand 2 Score=", sc2, sep=""))
    hand1 <- v[v$BullFightScore==sc1, ]
    if(dim(hand1)[1]==0) next
    
    # Case Hand 1 wins
    hand2 <- v[v$BullFightScore==sc2, ]
    if(dim(hand2)[1]==0) next
    
    # Merge the 2 hands
    hh <- merge(x=hand1, y=hand2, by=NULL)
    
    # Remove invalid combinations
    valid <- hh[,1:13] + hh[,18:30]
    valid <- rowSums( (valid > n.decks*4)*1 )==0
    hh <- hh[valid,]
    
    # Separate them
    df.hand1 <- hh[,1:17]
    df.hand2 <- hh[,18:34] # The Hand 2 Combinations are not correct as it assumes full deck, we need to correct
    
    #
    # Convert to 2 hands
    #
    # First 13 columns are the cards of the first hand
    freq.hand1 <- df.hand1[,1:13]
    no.card.choices.hand1 <- ( freq.hand1>-999999 ) * (n.decks*4)  # For each card, there is ndecks*4 of them
    no.card.choices.hand1[,1] <- n.decks * zerorankcount(nranks=13)
    
    freq.hand2 <- df.hand2[,1:13]
    no.card.choices.hand2 <- ( freq.hand2>-999999 ) * (n.decks*4)  # For each card, there is ndecks*4 of them
    no.card.choices.hand2[,1] <- n.decks * zerorankcount(nranks=13)
    no.card.choices.hand2 <- no.card.choices.hand2 - freq.hand1
    
    # Calculate the permutations of Hand 1 & 2
    perms.tmp1 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                        freq = freq.hand1, no.card.choices = no.card.choices.hand1)
    # Denominator
    den.tmp1 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                        freq = freq.hand1, no.card.choices = freq.hand1)
    # This should be the same as the original permutation
    df.hand1$Permutations.x <- factorial(5) * perms.tmp1 / den.tmp1
    # Sanity check
    stopifnot(sum((df.hand1$Permutations != df.hand1$Combinations.x)*1)==0)
    
    perms.tmp2 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                        freq = freq.hand2, no.card.choices = no.card.choices.hand2)
    # Denominator
    den.tmp2 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                      freq = freq.hand2, no.card.choices = freq.hand2)
    # This should NOT be the same as the original permutation (which assumes full deck)
    df.hand2$Permutations.y <- factorial(5) * perms.tmp2 / den.tmp2
    
    df.hand12 <- cbind(df.hand1[,14:18], df.hand2[,14:18])
    
    p = sum(df.hand12$Permutations.x * df.hand12$Permutations.y) / (52*51*50*49*48*47*46*45*44*43)
    prob.bf.matrix[sc1+1,sc2+1] <- p
    print(paste("   Probability=", p))
  }
}

# Copy over the symmetrical side
for(i in c(0:MAX.SCORE))
{
  for(j in c(i:MAX.SCORE))
  {
    if(i==j) next
    prob.bf.matrix[j+1,i+1] <- prob.bf.matrix[i+1,j+1]
  }
}
write.csv(x = prob.bf.matrix, file = "data/bullfight.matrix.csv")

# The win probability (of either hand) is just the sum of the win probability of Hand 2, minus the diagonals
p.bf.win <- ( sum(prob.bf.matrix) - sum(diag(prob.bf.matrix)) ) / 2
# The draw probability (of either hand) can be derived from symmetry of Hand 1 and 2
p.bf.draw <- 1 - 2*p.bf.win
p.bf.draw.00 <- p.bf.draw - sum(diag(prob.bf.matrix))
prob.bf.matrix[1,1] <- p.bf.draw.00

margin.bf.win <- 1 - (1.95*p.bf.win + 1.00*p.bf.draw)
margin.bf.draw <- 1 - (6.00*p.bf.draw)

#
# BullFight: Now calculate margins of other bets like Niu1, Niu2,... NiuNiu, ShuangNiuNiu, etc.
#
p.bf.niu <- rep(0, MAX.SCORE+1)
odds.bf.niu <- c(0, 6, 6, 6, 6, 6, 6, 6, 6, 6, 6, 0, 0, 0, 0, 0)
m.bf.niu <- rep(0, MAX.SCORE+1)
for(i in c(0:MAX.SCORE))
{
  # Sum of the row and column, minus the intersection cell
  p.bf.niu[i+1] <- sum(prob.bf.matrix[i+1,]) + sum(prob.bf.matrix[,i+1]) - prob.bf.matrix[i+1,i+1]
  m.bf.niu[i+1] <- (1 - odds.bf.niu[i+1]*p.bf.niu[i+1])
}

p.yinniu <- p.bf.niu[YINNIU.SCORE+1]
p.jinniu <- p.bf.niu[JINNIU_5GONG.SCORE+1]
p.zhadan <- p.bf.niu[ZHADAN.SCORE+1]
p.wuxiaoniu <- p.bf.niu[WUSHIAONIU.SCORE+1]

p.shuangniuniu <- prob.bf.matrix[11,11]
m.shuangniuniu <- 1 - (101*p.shuangniuniu)

intersection <- c(YINNIU.SCORE:MAX.SCORE) + 1
p.yinniu.jinniu.zhadan.wuxiaoniu <- (p.yinniu + p.jinniu + p.zhadan + p.wuxiaoniu) - sum(prob.bf.matrix[intersection,intersection])
m.yinniu.jinniu.zhadan.wuxiaoniu <- 1 - (121*p.yinniu.jinniu.zhadan.wuxiaoniu)

######################################################################################
# Bull Bull
######################################################################################

#
# BullBull: Calculate for Hand 2 win is good enough, due to symmetry
#
v <- v.bb
max.bb.score <- max(v$BullBullScore)
prob.bb.matrix <- matrix(data=0, nrow = max.bb.score+1, ncol = max.bb.score+1)
for(sc1 in c(0:max.bb.score))
{
  for(sc2 in c(sc1:max.bb.score))
  {
    # Don't do for (0,0), too many combinations, we will just estimate this one
    if(sc1==0 & sc2==0) next
    
    print(paste("Doing for Hand 1 Score=", sc1, ", Hand 2 Score=", sc2, sep=""))
    hand1 <- v[v$BullBullScore==sc1, ]
    if(dim(hand1)[1]==0) next
    
    # Case Hand 1 wins
    hand2 <- v[v$BullBullScore==sc2, ]
    if(dim(hand2)[1]==0) next
    
    # Merge the 2 hands
    hh <- merge(x=hand1, y=hand2, by=NULL)
    
    # Remove invalid combinations
    valid <- hh[,1:13] + hh[,18:30]
    valid <- rowSums( (valid > n.decks*4)*1 )==0
    hh <- hh[valid,]
    
    # Separate them
    df.hand1 <- hh[,1:17]
    df.hand2 <- hh[,18:34] # The Hand 2 Combinations are not correct as it assumes full deck, we need to correct
    
    #
    # Convert to 2 hands
    #
    # First 13 columns are the cards of the first hand
    freq.hand1 <- df.hand1[,1:13]
    no.card.choices.hand1 <- ( freq.hand1>-999999 ) * (n.decks*4)  # For each card, there is ndecks*4 of them
    no.card.choices.hand1[,1] <- n.decks * zerorankcount(nranks=13)
    
    freq.hand2 <- df.hand2[,1:13]
    no.card.choices.hand2 <- ( freq.hand2>-999999 ) * (n.decks*4)  # For each card, there is ndecks*4 of them
    no.card.choices.hand2[,1] <- n.decks * zerorankcount(nranks=13)
    no.card.choices.hand2 <- no.card.choices.hand2 - freq.hand1
    
    # Calculate the permutations of Hand 1 & 2
    perms.tmp1 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                        freq = freq.hand1, no.card.choices = no.card.choices.hand1)
    # Denominator
    den.tmp1 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                      freq = freq.hand1, no.card.choices = freq.hand1)
    # This should be the same as the original permutation
    df.hand1$Permutations.x <- factorial(5) * perms.tmp1 / den.tmp1
    # Sanity check
    stopifnot(sum((df.hand1$Permutations != df.hand1$Combinations.x)*1)==0)
    
    perms.tmp2 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                        freq = freq.hand2, no.card.choices = no.card.choices.hand2)
    # Denominator
    den.tmp2 <- get.card.permutations(ncards = 5, nranks = 13, ndecks = n.decks,
                                      freq = freq.hand2, no.card.choices = freq.hand2)
    # This should NOT be the same as the original permutation (which assumes full deck)
    df.hand2$Permutations.y <- factorial(5) * perms.tmp2 / den.tmp2
    
    df.hand12 <- cbind(df.hand1[,14:18], df.hand2[,14:18])
    
    p = sum(df.hand12$Permutations.x * df.hand12$Permutations.y) / (52*51*50*49*48*47*46*45*44*43)
    prob.bb.matrix[sc1+1,sc2+1] <- p
    print(paste("   Probability=", p))
  }
}

# Copy over the symmetrical side
for(i in c(0:max.bb.score))
{
  for(j in c(i:max.bb.score))
  {
    if(i==j) next
    prob.bb.matrix[j+1,i+1] <- prob.bb.matrix[i+1,j+1]
  }
}
write.csv(x = prob.bb.matrix, file = "data/bullbull.matrix.csv")

# The win probability (of either hand) is just the sum of the win probability of Hand 2, minus the diagonals
p.bb.win <- ( sum(prob.bb.matrix) - sum(diag(prob.bb.matrix)) ) / 2
# The draw probability (of either hand) can be derived from symmetry of Hand 1 and 2
p.bb.draw <- 1 - 2*p.bb.win
p.bb.draw.00 <- p.bb.draw - sum(diag(prob.bb.matrix))
prob.bb.matrix[1,1] <- p.bb.draw.00

margin.bb.win <- 1 - (1.95*p.bb.win + 1.00*p.bb.draw)
margin.bb.draw <- 1 - (6.00*p.bb.draw)

#
# Now estimate without using proper 2 hand combinations, by assuming infinite decks
# using only a single hand probability. The numbers should be close to the properly
# calculated one above.
#
v <- v.bb
p.fiveface <- sum(v$Combinations[v$BullBullScore==11]) / sum(v$Combinations)
p.bullbull <- sum(v$Combinations[v$BullBullScore==10]) / sum(v$Combinations)
p.bull9    <- sum(v$Combinations[v$BullBullScore==9 ]) / sum(v$Combinations)
p.bull8    <- sum(v$Combinations[v$BullBullScore==8 ]) / sum(v$Combinations)
p.bull7    <- sum(v$Combinations[v$BullBullScore==7 ]) / sum(v$Combinations)
p.bull6    <- sum(v$Combinations[v$BullBullScore==6 ]) / sum(v$Combinations)
p.bull5    <- sum(v$Combinations[v$BullBullScore==5 ]) / sum(v$Combinations)
p.bull4    <- sum(v$Combinations[v$BullBullScore==4 ]) / sum(v$Combinations)
p.bull3    <- sum(v$Combinations[v$BullBullScore==3 ]) / sum(v$Combinations)
p.bull2    <- sum(v$Combinations[v$BullBullScore==2 ]) / sum(v$Combinations)
p.bull1    <- sum(v$Combinations[v$BullBullScore==1 ]) / sum(v$Combinations)
p.nobull   <- sum(v$Combinations[v$BullBullScore==0 ]) / sum(v$Combinations)

p.win <- c(1:12)
p.lose <- c(1:12)
p.tie <- c(1:12)
for(i in c(1:12)) {
  pi        <- sum(v$Combinations[v$BullBullScore==i-1]) / sum(v$Combinations)
  pi.lower  <- sum(v$Combinations[v$BullBullScore<i-1]) / sum(v$Combinations)
  pi.higher <- sum(v$Combinations[v$BullBullScore>i-1]) / sum(v$Combinations)
  p.win[i] <- pi * pi.lower
  p.lose[i] <- pi * pi.higher
  p.tie[i] <- pi * pi
  
  print( paste("Bull ", (i-1), ": Prob Win=", p.win[i], ", Prob Lose=", p.lose[i], ", Prob Tie=", p.tie[i], sep="") )
}

p.win.bull79  <- p.win[7+1] + p.win[8+1] + p.win[9+1]
p.lose.bull79 <- p.lose[7+1] + p.lose[8+1] + p.lose[9+1]
p.tie.bull79  <- p.tie[7+1] + p.tie[8+1] + p.tie[9+1]

p.win.bullnormal  <- p.win[1+1] + p.win[2+1] + p.win[3+1] + p.win[4+1] + p.win[5+1] + p.win[6+1]
p.lose.bullnormal <- p.lose[1+1] + p.lose[2+1] + p.lose[3+1] + p.lose[4+1] + p.lose[5+1] + p.lose[6+1]
p.tie.bullnormal  <- p.tie[1+1] + p.tie[2+1] + p.tie[3+1] + p.tie[4+1] + p.tie[5+1] + p.tie[6+1]

p.tie.all <- p.fiveface*p.fiveface + p.bullbull*p.bullbull + p.tie.bull79 + p.tie.bullnormal + p.nobull*p.nobull
