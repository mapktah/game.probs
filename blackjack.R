
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/git/game.probs")
source("lib.card.blackjack.R")

rules <- bj.get.rules.structure.standard()
#rules$no.decks <- 4
#rules$can.surrender <- TRUE

playerstrategy <- BJ.STRATEGY.BASIC.DK8.EU.S17.DAS.NOSU
if(rules$no.decks == 4 & rules$can.surrender == TRUE)
  playerstrategy <- BJ.STRATEGY.BASIC.DK4.EU.S17.DAS.SU

byplayerhandvalue <- TRUE
verbose <- FALSE

# Generate Combinations
a=date()
bj.generate.player.combinations.all(rules = rules, playerstrategy = playerstrategy, byplayerhandvalue = byplayerhandvalue)
b=date()
bj.generate.dealer.combinations.all(rules = rules)
c=date()

#
# Test merging and calculations
#
dirpath.strategy  <- "combinations"
dirpath.player    <- "combinations"
dirpath.dealer    <- "combinations"
byplayerhandvalue <- TRUE
verbose <- TRUE
starttime <- date()
res <- bj.get.results.from.player.dealer.combinations(rules = rules,
                                                      dirpath.strategy = dirpath.strategy,
                                                      dirpath.player    = dirpath.player,
                                                      dirpath.dealer    = dirpath.dealer,
                                                      byplayerhandvalue = byplayerhandvalue,
                                                      verbose = verbose)
endtime <- date()
resmargin <- bj.get.stats.margins.from.result(df.result = res)

###################################################################################
#
# This was the less efficient calculation method done earlier. Both give same results.
#

source("lib.card.blackjack.combinations.R")

rules <- bj.get.rules.structure.standard()
playerstrategy <- BJ.STRATEGY.BASIC.WIKIPEDIA

deck.now=get.simple.deck(1,facecard = 10)
playerdf <- bj.get.player.attributes()
playerdf <- bj.add.card.to.player(rules=rules, newcard=1, df=playerdf)
deck.now <- reduce.deck(deck=deck.now, point=1, reduce.by=1)
playerdf <- bj.add.card.to.player(rules=rules, newcard=1, df=playerdf)
deck.now <- reduce.deck(deck=deck.now, point=1, reduce.by=1)
dealerdf <- bj.get.player.attributes(playertype="dealer")
dealerdf <- bj.add.card.to.player(rules=rules, newcard=1, df=dealerdf) ; deck.now <- reduce.deck(deck=deck.now, point=1, reduce.by=1)
p.combs.this = (4/52)*(3/51)*(2/50)
combsmultiplier = 1
use.ideal.probs <- FALSE
verbose <- FALSE
#
# Test dealer code
#
use.ideal.probs = FALSE
lstres <- bj.calculate.combinations.dealer(rules=rules, deck.now=deck.now, p.combs.this=p.combs.this, combsmultiplier=1,
                                           playerdf=playerdf, dealerdf=dealerdf,
                                           use.ideal.probs=use.ideal.probs)
lstres[["cardresults"]]
dc <- lstres[["cardcombinations"]]

lstres <- bj.calculate.combinations.player(rules=rules, deck.now=deck.now, p.combs.this=p.combs.this, combsmultiplier=combsmultiplier,
                                           playerdf=playerdf, dealerdf=dealerdf,
                                           playerstrategy=playerstrategy,
                                           use.ideal.probs=use.ideal.probs,
                                           verbose=verbose)
lstres[["cardresults"]]
dc <- lstres[["cardcombinations"]]

################
bj.calculate.dealer.bustoutrate <- function()
{
  for(dealerfacecard in c(2:10,1))
  {
    fname <- paste("combinations/combs.dealer.h17/full.dface.", dealerfacecard, ".csv", sep="")
    df <- read.csv(fname, header=TRUE)
    df.c <- bj.get.card.count(rules = rules, df=df, playertype = "dealer")
    freqcols <- paste("d.freq.", c(1:10), sep="")
    df.c$p.aggcount <- 1
    df.c$d.aggcount <- 1
    df.p <- bj.get.probability.counts(rules = rules, df=df.c, freqcols = freqcols)
    totalprob <- sum(df.p$Probability)
    dbust <- df.p[df.p$d.score>21,]
    print( paste("For DF=", dealerfacecard, ", bust prob=", (sum(dbust$Probability) / totalprob),sep="") )
  }
}

#
# The player bust out rate cannot be calculated from player combinations alone because it needs
# to first combine with dealer face cards (due to the strategy that depends on it),
# and the combination will have different probabilities.
#

#
# Analyze results from final combinations
#
bj.analyze.results.dealer <- function(dirpath.strategy, resulttype)
{
  df.result <- bj.read.all.results(dirpath.strategy, resulttype = resulttype)
  resana <- data.frame()
  
  rescols <- bj.get.result.columnnames()
  rescols <- gsub(pattern = "^Res", replacement = "", rescols)
  
  total.probability <- sum(df.result[,rescols])
  
  for(dealerfacecard in c(2:10,1))
  {
    dfs <- df.result[df.result$DF==dealerfacecard,]
    
    # Total relative probability
    relprob <- sum(dfs[,rescols])
    
    # Independent probability of dealer bust
    dealerbust <- sum(dfs$PlayerBJDealerBust + dfs$PlayerBustDealerBust + dfs$PlayerOkDealerBust)
    dealerbust.cond <- dealerbust / relprob
    
    resdf <- data.frame(DF=dealerfacecard, Probability=relprob, DBustCondProb=dealerbust.cond, DBustRealProb=dealerbust/total.probability)
    resana <- rbind(resana, resdf)
  }
  resana
}

bj.analyze.results.player <- function(rules, dirpath.strategy, resulttype)
{
  df.result <- bj.read.all.results(dirpath.strategy, resulttype = resulttype)
  resana <- data.frame()
  
  rescols <- bj.get.result.columnnames()
  rescols <- gsub(pattern = "^Res", replacement = "", rescols)
  
  total.probability <- sum(df.result[,rescols])
  
  # Derive a player hand value column
  df.result$PlayerHandValue <- ( df.result$P1 + df.result$P2 ) + 10 * ( df.result$P1==1 | df.result$P2==1 )
  
  for(playerhandvalue in c(4:21))
  {
    dfs <- df.result[df.result$PlayerHandValue==playerhandvalue,]
    
    # Total relative probability
    relprob <- sum(dfs[,rescols])
    
    # Independent probability of dealer bust
    playerbust <- sum(dfs$PlayerBustDealerBJ + dfs$PlayerBustDealerBust + dfs$PlayerBustDealerOk)
    playerbust.cond <- playerbust / relprob
    
    resdf <- data.frame(PlayerHandValue=playerhandvalue, Probability=relprob, PBustCondProb=playerbust.cond, PBustRealProb=playerbust/total.probability)
    resana <- rbind(resana, resdf)
  }
  resana
}
