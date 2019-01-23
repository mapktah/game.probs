
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

#
# 3 Card Baccarat
#
get.combination.count.3cardbaccarat.tablemethod <- function(n.decks)
{
  # Form a table of 6 columns (cards), ignore J-K, lump them as a 10-rank card
  v <- get.all.card.permutations(ncards=6, nranks=13, ndecks=n.decks)
  
  # Replace 0 with 13 (King)
  v$Card.1[v$Card.1==0] <- 13
  v$Card.2[v$Card.2==0] <- 13
  v$Card.3[v$Card.3==0] <- 13
  v$Card.4[v$Card.4==0] <- 13
  v$Card.5[v$Card.5==0] <- 13
  v$Card.6[v$Card.6==0] <- 13
  
  #
  # Face Card Count
  #
  v$PlayerFaceCard <- (v$Card.1>10)*1 + (v$Card.2>10)*1 + (v$Card.3>10)*1
  v$BankerFaceCard <- (v$Card.4>10)*1 + (v$Card.5>10)*1 + (v$Card.6>10)*1
  
  #
  # Sum of Card.1 & Card.2 = Player Score
  #
  Card1 <- v$Card.1
  Card2 <- v$Card.2
  Card3 <- v$Card.3
  FaceCardTotal <- v$PlayerFaceCard
  v$PlayerScore <- ( pmin(10,Card1) + pmin(10,Card2) + pmin(10,Card3) )%%10 +
    # If 3 face cards, score becomes 10, if max is K, additional 0.9, if Q additional 0.6, J additional 0.3
    (FaceCardTotal==3)*( 10 + 0*pmax(0,Card1-10,Card2-10,Card3-10) ) +
    # If 2 face cards, add 0.5, and if King is highest add another 0.3, if Queen another 0.2, and Jack additional 0.1
    # Then finally add another 0.01/0.02/0.03 if J/Q/K to settle cases of QQ vs QJ or KQ vs KJ
    (FaceCardTotal==2)*( 0.5 + 0*pmax(0,Card1-10,Card2-10,Card3-10) ) + 
    # For 1 Face Card
    (FaceCardTotal==1)*( 0.1 + 0*pmax(0,Card1-10,Card2-10,Card3-10) )
    
  Card1 <- v$Card.4
  Card2 <- v$Card.5
  Card3 <- v$Card.6
  FaceCardTotal <- v$BankerFaceCard
  v$BankerScore <- ( pmin(10,Card1) + pmin(10,Card2) + pmin(10,Card3) )%%10 +
    # If 3 face cards, score becomes 10, if max is K, additional 0.9, if Q additional 0.6, J additional 0.3
    (FaceCardTotal==3)*( 10 + 0*pmax(0,Card1-10,Card2-10,Card3-10) ) +
    # If 2 face cards, add 0.5, and if King is highest add another 0.3, if Queen another 0.2, and Jack additional 0.1
    # Then finally add another 0.01/0.02/0.03 if J/Q/K to settle cases of QQ vs QJ or KQ vs KJ
    (FaceCardTotal==2)*( 0.5 + 0*pmax(0,Card1-10,Card2-10,Card3-10) ) + 
    # For 1 Face Card
    (FaceCardTotal==1)*( 0.1 + 0*pmax(0,Card1-10,Card2-10,Card3-10) )
  
  v$Result <- (v$PlayerScore > v$BankerScore)*1 + (v$PlayerScore < v$BankerScore)*(-1)
  
  return(v)
}

#
# 3 Card Baccarat
#
v <- get.combination.count.3cardbaccarat.tablemethod(n.decks = 1)
p.3cb.player.win <- sum(v$Combinations[v$Valid & v$Result==1]) / sum(v$Combinations[v$Valid])
p.3cb.banker.win <- sum(v$Combinations[v$Valid & v$Result==-1]) / sum(v$Combinations[v$Valid])
p.3cb.tie <- sum(v$Combinations[v$Valid & v$Result==0]) / sum(v$Combinations[v$Valid])

for(i in c(0:10)) {
  tmp <- sum(v$Combinations[v$Valid & floor(v$PlayerScore)==i]) / sum(v$Combinations[v$Valid])
  print( paste( "Probability of point [", i, "] = ", tmp, sep="" ) )
}

# 3 Card Baccarat Super Six
p.3cbs6.player.win.not6 <- sum(v$Combinations[v$Valid & v$Result==1 & floor(v$PlayerScore)!=6]) / sum(v$Combinations[v$Valid])
p.3cbs6.player.win.6 <- sum(v$Combinations[v$Valid & v$Result==1 & floor(v$PlayerScore)==6]) / sum(v$Combinations[v$Valid])

# 3 Face
sum(v$Combinations[v$Valid & v$PlayerFaceCard==3]) / sum(v$Combinations[v$Valid])

