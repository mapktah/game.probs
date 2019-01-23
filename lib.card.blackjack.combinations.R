
source("lib.card.R")

####################################################################################
# Generate Combinations for Particular Strategies
####################################################################################

#
# Given a player/dealer data frame, we calculate the frequencies of aces,2,3,.., face cards.
# We then bind these frequencies to the original data frame and return them.
#
bj.get.card.count <- function(rules, df, playertype)
{
  colprefix <- "p."
  if(playertype=="dealer") colprefix <- "d."
  # Turn dealer column names into player column names
  colnames(df) <- gsub(pattern=paste("^",colprefix,sep=""), x=colnames(df), replacement="p.")
  
  # Total drawn cards may be more due to split
  df$p.totaldrawncards <- df$p.totalcards + df$p.splitlevel
  #df$p.factorialfactor <- calculate.total.permutations(df$p.totaldrawncards, rules$no.decks*52)

  min.pcard <- min(df$p.totalcards)
  max.pcard <- max(df$p.totalcards)
  
  df$Order <- c(1:dim(df)[1])
  df.new <- data.frame()
  
  for(i in c(min.pcard:max.pcard))
  {
    df.check <- df[df$p.totalcards==i,]
    
    # Extract player cards into columns
    pcards <- t(as.data.frame(strsplit(x=as.character(df.check$p.cardstring),split=",")))
    rownames(pcards) <- c(1:dim(pcards)[1])
    colnames(pcards) <- paste("p.C", c(1:dim(pcards)[2]), sep="")
    
    # Bind to main data frame
    df.cards <- data.frame(pcards)
    for(col in c(1:dim(df.cards)[2]))
      df.cards[,col] <- as.numeric(as.character(df.cards[,col]))
    
    # Count the frequencies of Ace, 2, 3, ... Face Cards
    df.cardscount <- data.frame()
    for(cardvalue in c(1:10))
    {
      colnew <- paste("p.freq.",cardvalue,sep="")
      # Mark those columns with card value
      tmp <- (df.cards==cardvalue)*1
      # Include splits
      freq.cardvalue <- rowSums(tmp) + (df.check$p.facecard==cardvalue)*df.check$p.splitlevel
      # If 1st column, need to create the data frame
      if(cardvalue==1) {
        df.cardscount <- data.frame(freq.cardvalue)
        colnames(df.cardscount) <- c(colnew)
      } else {
        df.cardscount[,colnew] <- freq.cardvalue
      }
    }
    # Bind to main frame
    df.check <- cbind(df.check, df.cardscount)
    
    df.new <- rbind(df.new, df.check)
  }
  
  # Encode the card frequencies so we can group them later (e.g. [2,5,7,2,2] and [2,2,2,5,7] are combinatorially equal)
  df.new$p.freq.encode <- as.character(df.new$p.freq.1)
  for(i in c(2:10)) {
    col <- paste("p.freq.",i,sep="")
    df.new$p.freq.encode <- paste(df.new$p.freq.encode, df.new[,col] ,sep=".")
  }
  
  # Turn back column names
  if(playertype=="dealer")
    colnames(df.new) <- gsub(pattern="^p.", x=colnames(df.new), replacement="d.")
  
  df.new <- df.new[order(df.new$Order, decreasing=FALSE),]
  df.new <- df.new[,setdiff(colnames(df.new), "Order")]
  
  # Sanity check
  if(dim(df.new)[1] != dim(df)[1])
    stop(paste("Error rows not equal nrow(df.new)=", dim(df.new)[1], ", nrow(df)=", dim(df)[1], sep=""))
  
  return(df.new)
}

#
# Compresses only needed information later from these permutations.
# This is key in making the calculations possible to execute.
#
bj.get.aggregated.combinations <- function(df, playertype)
{
  colprefix <- "p."
  if(playertype=="dealer") colprefix <- "d."
  # Turn dealer column names into player column names
  colnames(df) <- gsub(pattern=paste("^",colprefix,sep=""), x=colnames(df), replacement="p.")
  
  df$p.aggcount <- 1
  
  # Now we "compress" this information by aggregating by "p.score", "p.freq.encode", "p.totalcards", "p.splitlevel", "p.doublenomorehit"
  # because this is all that matters in combination calculations later
  aggcombs <- aggregate(x=data.frame(p.aggcount = df$p.aggcount),
                        by=list(p.score           = df$p.score,
                                p.totalcards      = df$p.totalcards,
                                p.splitlevel      = df$p.splitlevel,
                                p.doublenomorehit = df$p.doublenomorehit,
                                p.surrender       = df$p.surrender,
                                p.freq.encode     = df$p.freq.encode,
                                # We keep these columns for ease later when calculating combinations, not really needed since we already have p.freq.encode
                                p.freq.1          = df$p.freq.1,
                                p.freq.2          = df$p.freq.2,
                                p.freq.3          = df$p.freq.3,
                                p.freq.4          = df$p.freq.4,
                                p.freq.5          = df$p.freq.5,
                                p.freq.6          = df$p.freq.6,
                                p.freq.7          = df$p.freq.7,
                                p.freq.8          = df$p.freq.8,
                                p.freq.9          = df$p.freq.9,
                                p.freq.10         = df$p.freq.10
                        ),
                        FUN="sum")
  
  # Sanity check (total aggregate count equals rows of data frame)
  nrows <- dim(df)[1]
  aggcount <- sum(aggcombs$p.aggcount)
  if(nrows != aggcount)
    stop(paste("Error inconsistency!",sep=""))
  
  # Turn back column names
  if(playertype=="dealer")
    colnames(aggcombs) <- gsub(pattern="^p.", x=colnames(aggcombs), replacement="d.")
  
  return(aggcombs)
}

bj.generate.dealer.combinations.draw <- function(rules,
                                                 dealerdf,
                                                 verbose = FALSE)
{
  dfcombs  <- data.frame()
  
  # By default, stand
  dealerdf$nextdecision <- "S"
  
  # Hit if dealer score <= 16, or if score is soft 17 and <hitonsoft17> is TRUE
  if( dealerdf$score <= 16 | (dealerdf$score == 17 & dealerdf$hardsoft=="soft" & rules$dealer.hit.on.soft.17) ) {
    dealerdf$nextdecision <- "H"
  }
  
  # Stand & Player Bust, so we settle scores, no more calculations.
  if(dealerdf$nextdecision=="S")
  {
    # Keep card combinations
    combs.tmp <- bj.get.combination(pcombs=NA, combsmultiplier=NA, playerdf=bj.get.player.attributes(), dealerdf=dealerdf, type="dealer")
    return(combs.tmp)
  }
  
  for(nextcard in c(1:10))
  {
    dealerdf.tmp <- dealerdf
    dealerdf.tmp <- bj.add.card.to.player(rules=rules, newcard=nextcard, df=dealerdf.tmp)
    
    if(verbose) print(paste("Doing Combination [", dealerdf.tmp$cardstring, ", ", nextcard, "]", sep=""))
    
    combs.tmp <- bj.generate.dealer.combinations.draw(rules       = rules,
                                                      dealerdf    = dealerdf.tmp,
                                                      verbose     = verbose)
    dfcombs  <- rbind(dfcombs, combs.tmp)
  }
  
  return(dfcombs)
}

bj.generate.dealer.combinations <- function(rules, dealerfacecard, verbose=FALSE)
{
  dealerdf <- bj.get.player.attributes(playertype = "dealer")
  dealerdf <- bj.add.card.to.player(rules=rules, newcard=dealerfacecard, df=dealerdf)
  
  dfcombs  <- bj.generate.dealer.combinations.draw(rules, dealerdf=dealerdf, verbose=verbose)
  
  # Sanity check (all cards encoding must be unique)
  uniqcount <- length(unique(paste(dfcombs$d.cardsencoding, ".", dfcombs$d.cardsencoding11, ".", dfcombs$d.splitlevel, sep="")))
  if(uniqcount != dim(dfcombs)[1])
    stop(paste("Error in player combination generation, duplicate rows for dealerfacecard=", dealerfacecard, sep=""))
  
  return(dfcombs)
}

bj.generate.dealer.combinations.all <- function(rules, verbose=FALSE)
{
  for(dealerfacecard in c(1:10))
  {
    print(paste("Generating for Dealer Face Card ", dealerfacecard, sep=""))
    dfcombs <- bj.generate.dealer.combinations(rules, dealerfacecard = dealerfacecard, verbose = verbose)
    
    # Get card frequencies (how many aces, 2's, 3's,..)
    dfcombs.cardfreq <- bj.get.card.count(rules = rules, df = dfcombs, playertype = "dealer")
    
    aggcombs <- bj.get.aggregated.combinations(df = dfcombs.cardfreq, playertype = "dealer")
    
    # Write original to file
    fname <- paste("combinations/", "full.dface.", dealerfacecard, ".csv", sep="")
    write.csv(dfcombs, fname)
    
    # Write compressed/aggregated to file
    fname <- paste("combinations/", "agg.dface.", dealerfacecard, ".csv", sep="")
    write.csv(aggcombs, fname)
  }
}

#
# When <pscore> is soft, <pscore> passed in is where A=11
#
bj.generate.player.combinations.draw <- function(rules,
                                                 playerdf,
                                                 dealerdf,
                                                 playerstrategy,
                                                 verbose  = FALSE)
{
  dfcombs  <- data.frame()
  
  # Get next move decision (splits, doubles all done here also)
  playerdf.new <- bj.strategy.get.decision(rules=rules, playerstrategy=playerstrategy, playerdf=playerdf, dealerfacecard=dealerdf$facecard)
  # Just take 1st row (due to splits)
  playerdf.new <- playerdf.new[1,]
  
  if(playerdf.new$nextdecision=="H")
  {
    for(nextcard in c(1:10))
    {
      playerdf.tmp <- playerdf.new
      playerdf.tmp <- bj.add.card.to.player(rules=rules, newcard=nextcard, df=playerdf.tmp)
      
      combs.tmp <- bj.generate.player.combinations.draw(rules          = rules,
                                                        playerdf  = playerdf.tmp,
                                                        dealerdf       = dealerdf,
                                                        playerstrategy = playerstrategy,
                                                        verbose        = verbose)
      if(verbose) print(combs.tmp)
      dfcombs  <- rbind(dfcombs, combs.tmp)
    }
  }
  else if(playerdf.new$nextdecision=="S")
  {
    dfcombs <- bj.get.combination(pcombs=NA, combsmultiplier=NA, playerdf=playerdf.new, dealerdf=dealerdf, type="player")
  }
  return(dfcombs)
}

#
# Generate player combinations for a particular strategy, ignore number of decks.
# Number of decks can be handled later when we combine the player/dealer combinations.
#
bj.generate.player.combinations.all <- function(rules, playerstrategy, byplayerhandvalue=FALSE, verbose=FALSE)
{
  for(dealerfacecard in c(1:10))
  {
    dealerdf <- bj.get.player.attributes(playertype = "dealer")
    dealerdf <- bj.add.card.to.player(rules=rules, newcard=dealerfacecard, df=dealerdf)
    
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
        
        playerdf <- bj.get.player.attributes()
        playerdf <- bj.add.card.to.player(rules=rules, newcard=p1, df=playerdf)
        if(byplayerhandvalue)
          playerdf <- bj.add.card.to.player(rules=rules, newcard=p2, df=playerdf)
        
        print(paste("Generating player combinations for [DF=", dealerfacecard, ", P1=", p1, ", P2=", p2, "]", sep=""))
        
        dfcombs  <- bj.generate.player.combinations.draw(rules = rules,
                                                         playerdf=playerdf,
                                                         dealerdf=dealerdf,
                                                         playerstrategy=playerstrategy,
                                                         verbose=verbose)
        # Sanity check (all cards encoding must be unique)
        uniqcount <- length(unique(paste(dfcombs$p.cardsencoding, ".", dfcombs$p.cardsencoding11, ".", dfcombs$p.splitlevel, sep="")))
        if(uniqcount != dim(dfcombs)[1])
          stop(paste("Error in player combination generation, duplicate rows for p1=", p1,
                     ", dealerfacecard=", dealerfacecard, ", max split=", rules$max.split, sep=""))
        
        # Get card frequencies (how many aces, 2's, 3's,..)
        dfcombs.cardfreq <- bj.get.card.count(rules = rules, df = dfcombs, playertype = "player")
        
        aggcombs <- bj.get.aggregated.combinations(df = dfcombs.cardfreq, playertype = "player")
        
        # Write to CSV (Original)
        fname <- paste("combinations/", "full.dface.", dealerfacecard, ".p1.", p1, "-", p2, ".split.", rules$max.split, ".csv", sep="")
        write.csv(dfcombs.cardfreq, fname)
        
        # Write to CSV (Compressed/Aggregated)
        fname <- paste("combinations/", "agg.dface.", dealerfacecard, ".p1.", p1, "-", p2, ".split.", rules$max.split, ".csv", sep="")
        write.csv(aggcombs, fname)
      }
    }
  }
}

