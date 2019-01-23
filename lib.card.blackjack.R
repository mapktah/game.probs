
source("lib.card.R")
source("lib.card.blackjack.strategies.R")
source("lib.card.blackjack.combinations.R")

MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS <- 0.000000000000001

bj.merge.player.dealer <- function(rules, playerdf, dealerdf)
{
  lenp <- dim(playerdf)[1]
  lend <- dim(dealerdf)[1]
  playerdf$p.order <- c(1:lenp)
  dealerdf$d.order <- c(1:lend)
  df.merge <- merge(playerdf, dealerdf, by=NULL)
  
  # Check if merged properly (unique cartesian product of "order" columns must be the merged number of rows)
  xy <- df.merge$p.order * (1+log(max(lenp,lend, base=10))) + df.merge$d.order
  if(length(unique(xy)) != dim(df.merge)[1])
    stop("Error, did not merge properly.")
  
  # Get invalid rows & combinations
  p.freqcols <- paste("p.freq.", c(1:10), sep="")
  d.freqcols <- paste("d.freq.", c(1:10), sep="")
  df.p.freq <- df.merge[,p.freqcols]
  df.d.freq <- df.merge[,d.freqcols]
  df.freq <- df.p.freq + df.d.freq
  colnames(df.freq) <- paste("rank.freq.", c(1:10), sep="")
  # Condition not more than ranks in deck
  maxrank <- 4 * rules$no.decks
  valid <- matrix(data=maxrank, nrow=dim(df.freq)[1], ncol=dim(df.freq)[2])
  valid[,10] <- 16 * rules$no.decks
  isvalid <- !rowSums( x = ( 1 * ( df.freq > valid ) ) )
  
  # Put in total freq column
  df.merge <- cbind(df.merge, df.freq)
  # Remove player/dealer frequencies
  df.merge <- df.merge[,setdiff(colnames(df.merge),c(p.freqcols,d.freqcols))]
  df.merge$Valid <- isvalid
  
  return(df.merge)
}

bj.get.probability.counts <- function(rules, df, freqcols=c())
{
  # Extract the total cards frequency columns
  if(length(freqcols)==0)
    freqcols <- paste("rank.freq.", c(1:10), sep="")

  df.cardcounts <- df[,freqcols]
  
  nrow <- dim(df.cardcounts)[1]
  ncol <- dim(df.cardcounts)[2]
  # Get the combinations of this drawn player/dealer cards. From columns 1-9 rank, 4*n.decks cards available
  n <- matrix(data=c(rep(4*rules$no.decks, times=nrow*9), rep(16*rules$no.decks, times=nrow)), nrow=nrow, ncol=ncol)
  df$Combinations <- calculate.permutations(as.matrix(df.cardcounts), n)
  # Now get the factorial to divide by
  totaldrawncards <- rowSums(df.cardcounts)
  n <- matrix(data=rep(rules$no.decks*52,nrow), nrow=nrow, ncol=1)
  df$FactorialFactor <- calculate.permutations(as.matrix(totaldrawncards), n)
  
  df$Probability <- ( df$Combinations / df$FactorialFactor ) * ( df$p.aggcount * df$d.aggcount )
  return(df)
}

#
# We put the separately generated player and dealer combinations (without probability) together.
# The steps involved here are to mark invalid rows, get the probability counts, and retrieve game
# results.
#
bj.get.combinations.from.player.dealer.combinations <- function(rules,
                                                                dirpath.player,
                                                                dirpath.dealer,
                                                                dealerfacecard,
                                                                p1,
                                                                p2,
                                                                verbose   = FALSE)
{
  fname.playercombs <- paste(dirpath.player, "/agg.dface.", dealerfacecard, ".p1.", p1, "-", p2, ".split.", rules$max.split, ".csv", sep="")
  fname.dealercombs <- paste(dirpath.dealer, "/agg.dface.", dealerfacecard, ".csv", sep="")
  
  pc <- read.csv(file=fname.playercombs, header=TRUE)
  dc <- read.csv(file=fname.dealercombs, header=TRUE)
  
  # Do not merge <pc> and <dc>, R will hang!
  
  # We must segment to make the calculations complete in reasonable time.
  # Segment player scores
  segments <- c("22", "bj", "21")
  lpc <- list()
  ldc <- list()
  for(sg in segments)
  {
    if(sg == "22") {
      lpc[[sg]] <- pc[pc$p.score>=22, ]
      ldc[[sg]] <- dc[dc$d.score>=22, ]
    } else if(sg == "bj") {
      lpc[[sg]] <- pc[pc$p.score==21 & pc$p.totalcards==2, ]
      ldc[[sg]] <- dc[dc$d.score==21 & dc$d.totalcards==2, ]
    } else if(sg == "21") {
      lpc[[sg]] <- pc[(pc$p.score==21 & pc$p.totalcards>2) | pc$p.score<=20, ]
      ldc[[sg]] <- dc[(dc$d.score==21 & dc$d.totalcards>2) | dc$d.score<=20, ]
    }
  }
  # Sanity checks Player
  nrows <- 0
  for(sg in segments) nrows <- nrows + dim(lpc[[sg]])[1]
  if(dim(pc)[1] != nrows)
    stop("Player segment inconsistency!")
  
  # Sanity check Dealer
  nrows <- 0
  for(sg in segments) nrows <- nrows + dim(ldc[[sg]])[1]
  if(dim(dc)[1] != nrows)
    stop("Dealer segment inconsistency!")
  
  df.results.ret <- data.frame()
  
  for(ptype in segments)
  {
    dfplayer.tmp <- lpc[[ptype]]
    if(dim(dfplayer.tmp)[1]==0) next
    
    for(dtype in segments)
    {
      dfdealer.tmp <- ldc[[dtype]]
      if(verbose) print(paste("  [", dealerfacecard, ",", p1, ",", p2, "] Processing ptype [", ptype, "] , dtype [", dtype, "]..", sep=""))
      
      if(dim(dfdealer.tmp)[1]==0) next
      
      # Merge both and get card counts
      df.merge <- bj.merge.player.dealer(rules = rules, playerdf = dfplayer.tmp, dealerdf = dfdealer.tmp)
      # Remove invalid columns
      df.merge.valid <- df.merge[df.merge$Valid, ]
      
      # Do the probability counts
      df.merge.prob <- bj.get.probability.counts(rules = rules, df = df.merge.valid)
      
      # Finally count the win/lose/tie results
      df.results <- bj.get.results.of.player.dealer.combinations(rules = rules, df = df.merge.prob)
      df.results.ret <- rbind(df.results.ret, df.results)
    }
  }
  # Sanity check on probability
  total.prob    <- sum(df.results.ret$Probability)
  cards <- c(dealerfacecard, p1)
  if(byplayerhandvalue) cards <- c(cards, p2)
  expected.prob <- get.probability.cardsetpermutation(deck=get.simple.deck(ndecks=rules$no.decks, facecard=10), cards=cards )
  if(abs(total.prob-expected.prob) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
    stop(paste("Error. Expected prob=", expected.prob, ", but got ", total.prob,
               ". Dealer face card ", dealerfacecard,
               ", player face card ", p1, sep=""))
  
  return(df.results.ret)
}

#
# The final step in processing, we calculate the wins, losses, ties, etc.
# The first row in the result calculates only for the original hand,
# the second row includes results from doubles, splits.
#
bj.get.results.from.player.dealer.combinations <- function(rules,
                                                           dirpath.strategy,
                                                           dirpath.player,
                                                           dirpath.dealer,
                                                           byplayerhandvalue=FALSE,
                                                           verbose=FALSE)
{
  df.result <- data.frame()
  
  for(dealerfacecard in c(1:10))
  {
    df.result.df <- data.frame()
    
    # By default we don't generate by player hand value (first 2 dealt cards)
    allscores <- c(0)
    if(byplayerhandvalue) allscores <- c(2:21)
    
    for(pscore.hard in allscores)
    {
      p1.options <- 10
      if(pscore.hard > 0) p1.options <- floor(pscore.hard/2)
      
      for(p1 in c(1:p1.options))
      {
        p2 <- NA
        if(byplayerhandvalue)
        {
          p2 <- pscore.hard - p1
          # Impossible for a card to have more than 10 points
          if(p1 > 10 | p2 > 10) next
        }
        print(paste("Dealer Face Card = ", dealerfacecard, ", p1 = ", p1, ", p2 = ", p2, sep=""))
        
        res <- bj.get.combinations.from.player.dealer.combinations(rules             = rules,
                                                                   dirpath.player    = dirpath.player,
                                                                   dirpath.dealer    = dirpath.dealer,
                                                                   dealerfacecard    = dealerfacecard,
                                                                   p1                = p1,
                                                                   p2                = p2,
                                                                   verbose           = verbose)
        # Write results to file
        fname <- paste(dirpath.strategy, "/result/res.combinations.deck.", rules$no.decks, ".sp.", rules$max.split,
                       ".dface.", dealerfacecard, ".pcards.", p1, "-", p2, ".csv", sep="")
        write.csv(res, fname)
        
        total.probability <- sum(res$Probability)
        
        colres <- bj.get.result.columnnames()
        res.tmp <- res[,colres]
        # Pure result (only sum up columns with value greater than 0)
        df.result.tmp1 <- colSums(x=(1*(res.tmp>0)*res$Probability))
        # Including results of doubles & splits (sum up all values)
        df.result.tmp2 <- colSums(x=res.tmp*res$Probability)
        
        df.result.tmp <- rbind(df.result.tmp1, df.result.tmp2)
        colnames(df.result.tmp) <- gsub(pattern="^Res", replacement="", colnames(df.result.tmp))
        # Add metadata column names
        df.metadata <- data.frame(DK         = rep(rules$no.decks,2),
                                  DF         = rep(dealerfacecard,2),
                                  P1         = c(p1,p1),
                                  P2         = c(p2,p2),
                                  MaxSplit   = rep(rules$max.split,2),
                                  DAS        = rep(rules$double.after.split,2),
                                  SP1DRAW1   = rep(rules$split.ace.draw.one.more.card.only,2),
                                  LOSO       = rep(rules$split.lose.only.original.stake.if.dealer.bj,2),
                                  H17        = rep(rules$dealer.hit.on.soft.17,2),
                                  SU         = rep(rules$can.surrender,2),
                                  ResultType = c("Single","All"),
                                  TotalProb  = rep(total.probability,2))
        
        df.result.tmp <- cbind(df.metadata, df.result.tmp)
        
        # Generate symmetrical case if required
        if(byplayerhandvalue & p1<p2)
        {
          df.result.tmp2 <- df.result.tmp
          df.result.tmp2$P1 <- df.result.tmp$P2
          df.result.tmp2$P2 <- df.result.tmp$P1
          df.result.tmp <- rbind(df.result.tmp, df.result.tmp2)
        }
        
        # Write detailed results to results folder
        fname <- paste(dirpath.strategy, "/result/res.df.", dealerfacecard, ".p1.", p1, "-", p2, ".csv", sep="")
        write.csv(df.result.tmp, fname)

        # Update results
        df.result.df <- rbind(df.result.df, df.result.tmp)
      }
    }
    
    # For a particular dealer face card, write detailed results to results folder
    fname <- paste(dirpath.strategy, "/result/res.df.", dealerfacecard, ".csv", sep="")
    write.csv(df.result.df, fname)
    
    df.result <- rbind(df.result, df.result.df)
    # Sanity check on permutations for a given dealer face card
    total.prob <- sum(df.result$TotalProb[df.result$ResultType=="Single" & df.result$DF==dealerfacecard])
    # total.prob <- sum(df.result.df$TotalProb[df.result.df$ResultType=="Single"])
    cards <- c(dealerfacecard)
    expected.prob <- get.probability.cardsetpermutation(deck=get.simple.deck(ndecks=rules$no.decks, facecard=10), cards=cards )
    if(abs(total.prob-expected.prob) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
      stop(paste("Error. Expected prob=", expected.prob, ", but got ", total.prob,
                 ". Dealer face card ", dealerfacecard,
                 sep=""))
  }
  
  # Sanity check of first row results (total probability = 1)
  cols <- gsub("^Res", "", bj.get.result.columnnames())
  r1 <- df.result[df.result$ResultType=="Single",cols]
  r2 <- df.result[df.result$ResultType=="All",cols]
  if(abs(sum(r1)-1) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
    print(paste("Expected sum of probability is 1, but got ", sum(r1), sep=""))
  
  # Sanity check of first row results (BJ probability correct)
  # Probability of Player Blackjack (regardless of dealer)
  pbj <- sum(r1$PlayerBJDealerOk + r1$PlayerBJDealerBust + r1$PlayerBJDealerBJ)
  tc <- rules$no.decks * 52
  expected.pbj <- 2*4*16*rules$no.decks*rules$no.decks/(tc*(tc-1))
  if(abs(pbj - expected.pbj) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
    print(paste("Expected BJ probability = ", expected.pbj, " but got ", pbj, sep=""))
  
  # Write main results to top folder
  fname <- paste(dirpath.strategy, "/res.dk.", rules$no.decks, ".sp.", rules$max.split, ".das.", rules$double.after.split, ".csv", sep="")
  write.csv(df.result, fname)
  
  return(df.result)
}

bj.get.stats.margins.from.result <- function(df.result)
{
  # Extract Single/All rows
  res1 <- df.result[df.result$ResultType=="Single",]
  res2 <- df.result[df.result$ResultType=="All",]
  if(abs(sum(res1$TotalProb) - 1) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
    stop(paste("Error probability don't sum to 1, instead sum to ", sum(res1$TotalProb), sep=""))
  
  # Result columns
  rescols <- bj.get.result.columnnames()
  rescols <- gsub(pattern = "^Res", replacement = "", rescols)
  if(abs(sum(res1[,rescols]) - 1) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
    stop(paste("Error results probability don't sum to 1, instead sum to ", sum(res1[,rescols]), sep=""))
  
  # Total relative probability
  totalprob <- sum(res1[,rescols])
  if(abs(totalprob - 1) > MIN.TOLERANCE.SANITY.CHECK.COMBINATIONS)
    stop(paste("Error probability of results don't sum to 1, instead sum to ", sum(totalprob), sep=""))
  # Separate surrender from the rest
  res1.su <- res1[res1$SU,]
  res1.ot <- res1[!res1$SU,]
  # 5 Outcomes, WinBJ, WinOk, Lose, Push, LoseSu (lose surrender)
  p.win.bj <- sum(res1.ot$PlayerBJDealerBust)   + sum(res1.ot$PlayerBJDealerOk)
  p.win.ok <- sum(res1.ot$PlayerOkDealerOkWin)  + sum(res1.ot$PlayerOkDealerBust)
  p.bust   <- sum(res1.ot$PlayerBustDealerBJ)   + sum(res1.ot$PlayerBustDealerBust) + sum(res1.ot$PlayerBustDealerOk)
  p.lose   <- sum(res1.ot$PlayerOkDealerOkLose) + sum(res1.ot$PlayerOkDealerBJ)
  p.push   <- sum(res1.ot$PlayerBJDealerBJ)     + sum(res1.ot$PlayerOkDealerOkTie)
  p.su     <- 0
  if(dim(res1.su)[1] > 0) p.su     <- sum(res1.su[,rescols])
  p.win.bj + p.win.ok + p.bust + p.lose + p.push + p.su
  # Get margin (2.5 decimal odds blackjack, 2 normal win, 1 push, 0.5 surrender). This is not correct because
  # we don't handle splits/decimals. It is only indicative.
  expected.return <- ( p.win.bj*2.5 + p.win.ok*2 + p.bust*0 + p.lose*0 + p.push*1 + p.su*0.5 )
  margin <- 1 - expected.return
  retres1 <- data.frame(PlayerWinBJ=p.win.bj, PlayerWinOk=p.win.ok,
                        PlayerLoseBust=p.bust, PlayerLose=p.lose,
                        PlayerPush=p.push, Margin=margin,
                        ExpectedLoss=NA, TotalProb=totalprob)
  
  # Total relative probability
  totalprob <- sum(res2[,rescols])
  # Separate surrender from the rest
  res2.su <- res2[res2$SU,]
  res2.ot <- res2[!res2$SU,]
  # 5 Outcomes, WinBJ, WinOk, Lose, Push, LoseSu (lose surrender)
  p.win.bj <- sum(res2.ot$PlayerBJDealerBust)   + sum(res2.ot$PlayerBJDealerOk)
  p.win.ok <- sum(res2.ot$PlayerOkDealerOkWin)  + sum(res2.ot$PlayerOkDealerBust)
  p.bust   <- sum(res2.ot$PlayerBustDealerBJ)   + sum(res2.ot$PlayerBustDealerBust) + sum(res2.ot$PlayerBustDealerOk)
  p.lose   <- sum(res2.ot$PlayerOkDealerOkLose) + sum(res2.ot$PlayerOkDealerBJ)
  p.push   <- sum(res2.ot$PlayerBJDealerBJ)     + sum(res2.ot$PlayerOkDealerOkTie)
  p.su     <- 0
  if(dim(res2.su)[1] > 0) p.su <- sum(res2.su[,rescols])
  p.win.bj + p.win.ok + p.bust + p.lose + p.push + p.su
  
  # Get margin (2.5 decimal odds blackjack, 2 normal win, 1 push, 0.5 surrender)
  expected.return <- ( p.win.bj*2.5 + p.win.ok*2 + p.bust*0 + p.lose*0 + p.push*1 + p.su*0.5 )
  margin <- (totalprob - expected.return) / totalprob
  # With Probability summing up to 1
  retres2 <- data.frame(PlayerWinBJ=p.win.bj/totalprob, PlayerWinOk=p.win.ok/totalprob,
                        PlayerLoseBust=p.bust/totalprob, PlayerLose=p.lose/totalprob,
                        PlayerPush=p.push/totalprob, Margin=margin,
                        ExpectedLoss=(totalprob-expected.return), TotalProb=totalprob)
  retres <- rbind(retres1, retres2)
  return(retres)
}
