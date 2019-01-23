source("lib.card.R")
source("lib.card.blackjack.strategies.R")

bj.generate.combinations.dealer <- function(deck.now,
                                            playerdf,
                                            dealerdf,
                                            hitonsoft17     = FALSE,
                                            use.ideal.probs = FALSE,
                                            verbose         = FALSE)
{
  # Dealer will keep drawing as long as he has 16 or less.
  # Hitting on a soft 17 depends on the <hitonsoft17> flag.
  # First row keeps results of original hand, 2nd row keeps hands doubled or split
  dfresult <- bj.get.result.structure()
  
  # By default, stand
  hit <- FALSE
  
  # Hit if dealer score <= 16, or if score is soft 17 and <hitonsoft17> is TRUE
  if( dealerdf$dscore <= 16 | (dealerdf$dscore == 17 & dealerdf$dhardsoft=="soft" & hitonsoft17) ) {
    hit <- TRUE
  }
  
  # Stand, so we settle scores, no more calculations.
  if(!hit)
  {
    totalplayerhands <- dim(playerdf)[1]
    for(i in c(1:totalplayerhands))
    {
      dfresult.tmp <- bj.get.result.structure()
      
      # Blackjack case
      if(dealerdf$dscore==21 & dealerdf$dtotalcards==2)
      {
        # BJ Tie
        if(playerdf$pscore[i]==21 & playerdf$ptotalcards[i]==2) dfresult.tmp$TieBJ[1+(i>1)] <- 1
        # Dealer Wins BJ
        else dfresult.tmp$DealerWinBJ[1+(i>1)] <- 1
      }
      # Player Wins BJ
      else if(playerdf$pscore[i]==21 & playerdf$ptotalcards[i]==2)
      {
        dfresult.tmp$PlayerWinBJ[1+(i>1)] <- 1
      }
      # Bust
      else if(playerdf$pscore[i] > 21 | dealerdf$dscore > 21)
      {
        if(dealerdf$dscore <= 21)
          dfresult.tmp$DealerWin[1+(i>1)] <- 1
        else if(playerdf$pscore[i] <= 21)
          dfresult.tmp$PlayerWin[1+(i>1)] <- 1
        else
          dfresult.tmp$Tie[1+(i>1)] <- 1
      }
      else {
        # Normal cases
        if(dealerdf$dscore > playerdf$pscore[i]) {
          dfresult.tmp$DealerWin[1+(i>1)] <- 1
        } else if(dealerdf$dscore == playerdf$pscore[i]) {
          dfresult.tmp$Tie[1+(i>1)] <- 1
        } else {
          dfresult.tmp$PlayerWin[1+(i>1)] <- 1
        }
      }
      # If player doubled / split, copy to Hand 2 results also
      if(playerdf$pnomorehit[i])
      {
        if(i==1)
          dfresult.tmp[2,] < dfresult.tmp[1,]
        else
          dfresult.tmp[2,] < dfresult.tmp[2,] * 2
      }
      
      dfresult <- dfresult + dfresult.tmp
    }
    return(dfresult)
  }
  
  for(nextcard in c(1:10))
  {
    deck.tmp <- deck.now
    dealerdf.tmp <- dealerdf
    dealerdf.tmp$dtotalcards <- dealerdf.tmp$dtotalcards + 1
    
    p.combs <- get.card.count.in.deck(deck=deck.tmp, point=nextcard) / sum(deck.tmp$TotalCards)
    if(use.ideal.probs) p.combs <- ( ( (nextcard==10) * 12 ) + 4 ) / 52
    
    if(!use.ideal.probs) {
      # Try to reduce deck
      deck.tmp <- reduce.deck(deck=deck.tmp, point=nextcard, reduce.by=1)
      # Not enough cards
      if(dim(deck.tmp)[1] == 0) next
    }
    
    # Dealer will not bust if hard, since we already made sure with maxcard
    dscore.tmp <- dealerdf.tmp$dscore + nextcard
    
    if(dealerdf.tmp$dhardsoft=="hard" & dscore.tmp>21)
    {
      dealerdf.tmp$dscore <- dscore.tmp
    }
    else
    {
      # Dealer previously has soft score and bust with A=11
      if( (dealerdf.tmp$dhardsoft == "soft") & (dscore.tmp > 21) ) {
        # Ace becomes 1 again
        dscore.tmp <- dscore.tmp - 10
        dealerdf.tmp$dhardsoft <- "hard"
      }
      
      # If Ace drawn & A=11 don't bust
      if( (nextcard==1) & (dscore.tmp+10 <=21) )
      {
        # Dealer has soft score now
        dscore.tmp <- dscore.tmp + 10
        dealerdf.tmp$dhardsoft <- "soft"
      }
      
      dealerdf.tmp$dscore <- dscore.tmp
    }
    
    combs.tmp <- bj.calculate.combinations.dealer.multihands(deck.now        = deck.tmp,
                                                             playerdf        = playerdf,
                                                             dealerdf        = dealerdf.tmp,
                                                             hitonsoft17     = hitonsoft17,
                                                             use.ideal.probs = use.ideal.probs,
                                                             verbose         = verbose)
    dfresult <- dfresult + ( p.combs * combs.tmp )
  }
  
  return(dfresult)
}

#
# When <pscore> is soft, <pscore> passed in is where A=11
#
bj.generate.combinations <- function(deck.now,
                                     playerdf,
                                     playerstrategy,
                                     dealerdf,
                                     use.ideal.probs = FALSE,
                                     verbose         = FALSE)
{
  # First row keeps results of original hand, 2nd row keeps hands doubled or split
  dfresult <- bj.get.result.structure()
  
  # Get next move decision (splits, doubles all done here also)
  playerdf.new <- bj.strategy.get.decision(playerstrategy=playerstrategy, playerdf=playerdf, dealerfacecard=dealerdf$dfacecard)
  if(verbose) print(playerdf.new)
  
  # Check if all hands "S"
  all.stand <- ( paste( unique(playerdf.new$pnextdecision), collapse="" ) == "S" )
  
  # Now we deal cards to all player hands
  if(!all.stand)
  {
    totalhands <- dim(playerdf.new)[1]
    for(i in c(1:totalhands))
    {
      if(playerdf.new$pnextdecision[i] == "H")
      {
        for(nextcard in c(1:10))
        {
          deck.tmp <- deck.now
          playerdf.tmp <- playerdf.new
          playerdf.tmp$ptotalcards[i] <- playerdf.tmp$ptotalcards[i] + 1
          
          p.combs <- ( get.card.count.in.deck(deck=deck.tmp, point=nextcard) / sum(deck.tmp$TotalCards) )
          if(use.ideal.probs) p.combs <- ( ( (nextcard==10) * 12 ) + 4 ) / 52
          
          if(!use.ideal.probs) {
            # Try to reduce deck
            deck.tmp <- reduce.deck(deck=deck.tmp, point=nextcard, reduce.by=1)
            # Not enough cards
            if(dim(deck.tmp)[1] == 0) next
          }
          
          # Player will not bust, since we already made sure the maxcard value
          pscore.hit <- playerdf.tmp$pscore[i] + nextcard
          
          # Player bust
          # This is where the dealer advantage comes in, there is no tie if dealer busts later,
          # player immediately loses if he busts.
          if(playerdf.tmp$phardsoft[i]=="hard" & pscore.hit>21)
          {
            playerdf.tmp$pscore[i] <- pscore.hit
          }
          else
          {
            # Player previously has soft score and bust with A=11
            if( (playerdf.tmp$phardsoft[i] == "soft") & (pscore.hit > 21) ) {
              # Ace becomes 1 again
              pscore.hit <- pscore.hit - 10
              playerdf.tmp$phardsoft[i] <- "hard"
            }
            
            # If Ace drawn & A=11 don't bust
            if(nextcard==1 & pscore.hit+10 <=21)
            {
              # Soft score takes priority
              pscore.hit <- pscore.hit + 10
              playerdf.tmp$phardsoft[i] <- "soft"
            }
            playerdf.tmp$pscore[i]      <- pscore.hit
            playerdf.tmp$pscore.hard[i] <- pscore.hit - 10*(playerdf.tmp$phardsoft[i]=="soft")
          }
          
          if(verbose) print(paste("  Player [", i, "]  Draw [", nextcard, "], Score = ", playerdf.tmp$pscore.hard[i],
                                  ", Soft/Hard = ", playerdf.tmp$phardsoft[i], sep=""))
          
          combs.tmp <- bj.calculate.combinations.player.multihands(deck.now        = deck.tmp,
                                                                   playerdf        = playerdf.tmp,
                                                                   playerstrategy  = playerstrategy,
                                                                   dealerdf        = dealerdf,
                                                                   use.ideal.probs = use.ideal.probs,
                                                                   verbose         = verbose)
          
          dfresult <- dfresult + (p.combs * combs.tmp)
        }
      }
    }
  }
  else
  {
    p.combs <- 1
    
    # Dealer's turn to draw card
    combs.tmp <- bj.calculate.combinations.dealer.multihands(deck.now        = deck.now,
                                                             playerdf        = playerdf,
                                                             dealerdf        = dealerdf,
                                                             hitonsoft17     = FALSE,
                                                             use.ideal.probs = use.ideal.probs,
                                                             verbose         = verbose)
    
    dfresult <- dfresult + (p.combs * combs.tmp)
  }
  
  return(dfresult)
}  
