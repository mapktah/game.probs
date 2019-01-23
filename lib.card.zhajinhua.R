
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

ndecks <- 1
n <- ndecks * 52
# Total combination of six cards
totalcom <- n * (n-1) * (n-2) * (n-3) * (n-4) * (n-5) / factorial(6)
# Total permutation of six cards
totalper <- n * (n-1) * (n-2) * (n-3) * (n-4) * (n-5)


get.prob.winner.3ofakind <- function()
{
  # Probability of Dragon 3OAK, Phoenix anything
  prob.dragon.3ofakind <- 13*4*3*2*(n-3)*(n-4)*(n-5) / totalper
  # Probability of Dragon losing with 3OAK
  prob.dragon.lose.3ofakind <- 0
  
  for(rank in c(2:14)) # 14 is Ace
  {
    # Total permutations of 3-of-a-kind of this rank
    per.3ofakind <- 4*3*2
    
    # Total permutations of 3-of-a-kind higher than it
    per.phoenix.higher.3ofakind <- (14-rank) * per.3ofakind
    
    # Total permutations of 2-3-5 (this combination beats 3-of-a-kind)
    per.phoenix.235 <- 4*4*4 * factorial(3)
    if(is.element(rank, c(2,3,5)))
      per.phoenix.235 <- 3*4*4 * factorial(3)
    
    prob.dragon.lose.3ofakind <- prob.dragon.lose.3ofakind +
      ( per.3ofakind * (per.phoenix.higher.3ofakind + per.phoenix.235) / totalper )
  }
  
  # Thus probability of Dragon winning with 3-of-a-kind
  prob.dragon.win.3ofakind <- prob.dragon.3ofakind - prob.dragon.lose.3ofakind
  
  # Due to symmetry of Dragon/Phoenix, the probability of Phoenix winning with 3 of a kind is the same.
  # Thus the probability of any Dragon/Phoenix winning with 3-of-a-kind
  prob.win.3ofakind <- prob.dragon.win.3ofakind * 2
  
  return(prob.win.3ofakind)
}

get.prob.winner.straightflush <- function()
{
  # Probability of dragon straight flush, Phoenix anything
  prob.dragon.sf <- 12 * (12*2*1) * (n-3)*(n-4)*(n-5) / totalper
  prob.dragon.lose.sf <- 0
  prob.dragon.draw.sf <- 0
  
  # Loop from low card Ace to Queen
  for(c1 in c(1:12))
  {
    # Permutations of straight flush with c1, c1+1, c1+2
    per.dragon.sf.c1 <- 12*2*1
    
    # Permutations of Phoenix with higher straight flush
    per.phoenix.sf.higher <- ( max(0,12-c1-2) * 12*2*1 ) + # SF higher with no rank overlap
                    ( (c1<=11) * 3*1*1 * factorial(3)  ) + # SF higher with 1 rank overlap
                    ( (c1<=10) * 3*1*1 * factorial(3)  )   # SF higher with 2 rank overlap
    
    # Permutations of Phoenix higher with 3-of-a-kind
    per.phoenix.3oak <- ( 10 * 4*3*2 ) + # 3OAK with no rank overlap
                    ( 3 * 3*2*1  ) # 3OAK with rank overlap
    
    # Add Probability of dragon losing with SF with c1,.. SF to cumulative probability.
    prob.dragon.lose.sf <- prob.dragon.lose.sf +
      (per.dragon.sf.c1 * (per.phoenix.sf.higher + per.phoenix.3oak) / totalper)
    
    #
    # Draw Case
    #
    prob.dragon.draw.sf <- prob.dragon.draw.sf + ( (per.dragon.sf.c1 * 9*2*1) / totalper )
  }
  
  prob.dragon.win.sf <- prob.dragon.sf - prob.dragon.lose.sf - prob.dragon.draw.sf

  # Due to symmetry of Dragon/Phoenix, the probability of Phoenix winning with SF is the same.
  # Thus the probability of any Dragon/Phoenix winning with SF
  prob.win.sf <- prob.dragon.win.sf * 2
  
  return(prob.win.sf)
}

get.prob.winner.flush <- function()
{
  # Probability of Dragon normal flush, Phoenix anything
  prob.dragon.flush <- (52*12*11 * (n-3)*(n-4)*(n-5) / totalper) - # All Flush
    (12 * (12*2*1) * (n-3)*(n-4)*(n-5) / totalper)                 # Straight Flush
  prob.dragon.lose.flush <- 0
  prob.dragon.draw.flush <- 0
  
  # Permutations of Phoenix with higher normal flush and all straight flush
  prob.phoenix.higher.flush <- 0
  prob.phoenix.higher.sf    <- 0
  prob.phoenix.higher.3OAK  <- 0
  prob.phoenix.tie.flush    <- 0
  
  prob.tmp <- 0
  
  # Flush has no pairs for Single Deck
  # Loop from low card 2 to Jack (low card can't be Queen, otherwise with King/Ace it will be straight flush)
  for(c1 in c(2:11))
  {
    for(c2 in c((c1+1):13)) # 2nd card can be up to King
    {
      for(c3 in c((c2+1):14)) # 14 is Ace
      {
        # Only flush, no straight flush
        if((c2==c1+1 & c3==c2+1) | (c3==14 & c1==2 & c2==3)) next

        dcards <- c(c1,c2,c3)
        # Permutations of this flush 3-set card c1,c2,c3
        per.dragon.flush.c1 <- 4*1*1 * factorial(3)
        # Sanity check variable, must be equal to prob.dragon.flush
        prob.tmp = prob.tmp + (per.dragon.flush.c1 * (n-3)*(n-4)*(n-5) / totalper)
          
        #
        # PHOENIX CARDS
        # Permutations of Phoenix with higher 3OAK, flush (& straight flush), straight
        #
        # Calculate first for Phoenix 3OAK (235 beats 3OAK)
        if(!(c1==2 & c2==3 & c3==5))
        {
          per.phoenix.higher.3OAK <-
            ( ( 10 * (4*3*2) * factorial(3) ) / totalper ) + # No rank overlap
            ( ( 3 * (3*2*1) * factorial(3) ) / totalper )    # With rank overlap
          prob.phoenix.higher.3OAK <- prob.phoenix.higher.3OAK + ( per.dragon.flush.c1 * per.phoenix.higher.3OAK / totalper )
        }
        
        # Now do for Phoenix Flushes / Straight
        # Loop from low card 2 to Queen
        for(pc1 in c(1:12))
        {
          for(pc2 in c((pc1+1):13)) # 2nd card can be up to King
          {
            for(pc3 in c((pc2+1):14)) # 14 is Ace
            {
              # 1 (Ace) only used for case of 1-2-3 straight
              if( pc1==1 & !(pc2==2 & pc3==3) ) next
              
              # CASE OF PHOENIX STRAIGHT FLUSH
              if(pc2==pc1+1 & pc3==pc2+1)
              {
                # Straight Flush
                # print(paste("SF Phoenix (",pc1,",",pc2,",",pc3,")", " Dragon (",c1,",",c2,",",c3,")", sep=""))
                per.phoenix.higher.sf <- ( 3*1*1 * factorial(3) )
                prob.phoenix.higher.sf <- prob.phoenix.higher.sf +
                  ( ( per.dragon.flush.c1 * per.phoenix.higher.sf ) / totalper )
                next
              }
              # Case of Tie
              else if(pc1==c1 & pc2==c2 & pc3==c3)
              {
                # print(paste("Tie Flush Phoenix (",pc1,",",pc2,",",pc3,")", " Dragon (",c1,",",c2,",",c3,")", sep=""))
                per.phoenix.tie.flush <-
                  ( 3*1*1* factorial(3) )
                prob.phoenix.tie.flush <- prob.phoenix.tie.flush +
                  ( ( per.dragon.flush.c1 * per.phoenix.tie.flush ) / totalper )
              }
              # Case of Phoenix Normal Flush
              else if ( (pc3>c3) | (pc3==c3 & pc2>c2) | (pc3==c3 & pc2==c2 & pc1>c1) )
              {
                # print(paste("Higher Flush Phoenix (",pc1,",",pc2,",",pc3,")", " Dragon (",c1,",",c2,",",c3,")", sep=""))
                per.phoenix.higher.flush <-
                  ( 3*1*1* factorial(3) )
                prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                  ( ( per.dragon.flush.c1 * per.phoenix.higher.flush ) / totalper )
              }
            }
          }
        }
      }
    }
  }
  
  stopifnot( abs(prob.dragon.flush - prob.tmp) < 0.000000001 )
  prob.dragon.win.flush <- prob.dragon.flush - 
    prob.phoenix.higher.flush - prob.phoenix.higher.sf -
    prob.phoenix.higher.3OAK - prob.phoenix.tie.flush

  # Due to symmetry of Dragon/Phoenix, the probability of Phoenix winning with SF is the same.
  # Thus the probability of any Dragon/Phoenix winning with SF
  prob.win.flush <- prob.dragon.win.flush * 2
  prob.win.flush

  return(prob.win.flush)
}

get.prob.winner.straight <- function()
{
  # Probability of dragon straight (but not straight flush), Phoenix anything
  prob.dragon.straight <- ( 12 * (12*8*4) * (n-3)*(n-4)*(n-5) / totalper ) - # All Straight
    ( 12 * (12*2*1) * (n-3)*(n-4)*(n-5) / totalper )                        # Straight Flush
  prob.dragon.lose.straight <- 0
  prob.dragon.draw.straight <- 0
  
  # Permutations of Phoenix with higher normal flush and all straight flush
  prob.phoenix.higher.straight.sf <- 0
  prob.phoenix.higher.flush <- 0
  prob.phoenix.higher.3OAK  <- 0
  prob.phoenix.tie.straight <- 0
  
  prob.tmp <- 0
  
  # Loop from 1 to Jack (low card can't be Queen, otherwise with King/Ace it will be straight flush)
  for(c1 in c(1:12))
  {
    for(c2 in c((c1+1):13)) # 2nd card can be up to King
    {
      for(c3 in c((c2+1):14)) # 14 is Ace
      {
        # Card value 1 only used for straight A,2,3
        if( c1==1 & !(c2==2 & c3==3) ) next
        # Only straight
        if( ! (c2==c1+1 & c3==c2+1) ) next
        
        dcards <- c(c1,c2,c3)
        # Permutations of this straight without SF
        per.dragon.straight.c1 <- ( 4*4*4 * factorial(3)) - ( 4*1*1 * factorial(3) )
        per.dragon.straight.c1.suits.11x     <- ( 4*1*3 * factorial(3)) # Same suit on first 2 low cards
        per.dragon.straight.c1.suits.x11     <- per.dragon.straight.c1.suits.11x # Same suit on top 2 high cards
        per.dragon.straight.c1.suits.1x1     <- per.dragon.straight.c1.suits.11x # Same suit on low and hi card
        per.dragon.straight.c1.suits.123     <- ( 4*3*2 * factorial(3)) # All different suits
        # Sanity check
        stopifnot(per.dragon.straight.c1 == per.dragon.straight.c1.suits.11x + per.dragon.straight.c1.suits.x11 +
                    per.dragon.straight.c1.suits.1x1 + per.dragon.straight.c1.suits.123)
        # Sanity check variable, must be equal to prob.dragon.straight
        prob.tmp = prob.tmp + (per.dragon.straight.c1 * (n-3)*(n-4)*(n-5) / totalper)
        
        #
        # PHOENIX CARDS
        # Permutations of Phoenix with higher 3OAK, flush (& straight flush), straight
        #
        is.dragon.235 <- (c1==2 & c2==3 & c3==5)
        # Calculate first for Phoenix 3OAK (235 beats 3OAK)
        if(!is.dragon.235)
        {
          per.phoenix.higher.3OAK <-
            ( ( 10 * (4*3*2) ) / totalper ) + # 3OAK with no rank overlap
            ( ( 3 * (3*2*1) ) / totalper )    # 3OAK with rank overlap
          prob.phoenix.higher.3OAK <- prob.phoenix.higher.3OAK + ( per.dragon.straight.c1 * per.phoenix.higher.3OAK / totalper )
        }
        
        # Now do for Phoenix Flushes / Straight
        # Loop from low card 2 to Queen
        for(pc1 in c(1:12))
        {
          for(pc2 in c(pc1:13)) # 2nd card can be up to King
          {
            for(pc3 in c(pc2:14)) # 14 is Ace
            {
              # 1 (Ace) only used for case of 1-2-3 straight
              if( pc1==1 & !(pc2==2 & pc3==3) ) next
              
              flag.phoenix.unique          <- (pc1!=pc2 & pc2!=pc3 & pc1!=pc3)
              
              flag.phoenix.straight        <- (pc2==pc1+1 & pc3==pc2+1)
              flag.phoenix.straight.higher <- flag.phoenix.straight & pc3>c3
              flag.phoenix.straight.tie    <- flag.phoenix.straight & pc3==c3
              flag.phoenix.straight.lower  <- flag.phoenix.straight & pc3<c3
              
              flag.phoenix.triple          <- (pc1==pc2 & pc2==pc3)
              flag.phoenix.pair            <- !flag.phoenix.unique & !flag.phoenix.triple
              # For case of normal flush
              flag.phoenix.hicard          <- ( flag.phoenix.unique &
                                                 !flag.phoenix.straight & !flag.phoenix.triple & !flag.phoenix.pair )

              # Case of Phoenix SF with lower straight
              if(flag.phoenix.straight.lower)
              {
                # If lo card of Dragon greater than hi card of Phoenix
                if(c1>pc3)
                {
                  # No rank overlap, Phoenix SF any suit
                  prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf +
                    ( (4*1*1) * factorial(3) * per.dragon.straight.c1 ) / totalper
                } else if(c1==pc3) {
                  # 1 rank overlap, Phoenix SF 3 possible suits
                  prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf +
                    ( (3*1*1) * factorial(3) * per.dragon.straight.c1 ) / totalper
                } else {
                  prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf +
                    # 2 rank overlaps, 1 suit overlap
                    ( (3*1*1) * factorial(3) * per.dragon.straight.c1.suits.11x ) / totalper +
                    # 2 rank overlaps, 2 suit overlaps
                    ( (2*1*1) * factorial(3) * (per.dragon.straight.c1.suits.1x1 + per.dragon.straight.c1.suits.x11 + per.dragon.straight.c1.suits.123) ) /totalper
                }
              }
              # Case of Tie
              else if(flag.phoenix.straight.tie)
              {
                prob.phoenix.higher.straight.sf.tiecards <-
                  # Dragon 2 suits, Phoenix SF any 2 suits
                  ( (2*1*1) * factorial(3) * (per.dragon.straight.c1.suits.11x + per.dragon.straight.c1.suits.1x1 + per.dragon.straight.c1.suits.x11) ) / totalper +
                  # Dragon 3 suits, Phoenix SF 1 suit
                  ( (3*1*1) * factorial(3) * (per.dragon.straight.c1.suits.123) ) / totalper
                prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf + prob.phoenix.higher.straight.sf.tiecards
                
                prob.phoenix.tie.straight.tiecards <- ( ( 3*3*3 * factorial(3) * per.dragon.straight.c1 ) / totalper ) -
                  prob.phoenix.higher.straight.sf.tiecards
                prob.phoenix.tie.straight <- prob.phoenix.tie.straight + prob.phoenix.tie.straight.tiecards
                  
              }
              else if (flag.phoenix.straight.higher)
              {
                if(pc1>c3) {
                  # No rank overlap
                  prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf +
                    ( 4*4*4 * factorial(3) * per.dragon.straight.c1 ) / totalper
                } else if (pc1==c3) {
                  # 1 rank overlap
                  prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf +
                    ( 3*4*4 * factorial(3) * per.dragon.straight.c1 ) / totalper
                }
                else if (pc1==c2) {
                  prob.phoenix.higher.straight.sf <- prob.phoenix.higher.straight.sf +
                    ( 3*3*4 * factorial(3) * per.dragon.straight.c1 ) / totalper
                }
              }
              else if (flag.phoenix.hicard)
              {
                # No need to care about rank overlap, just suits
                pcards <- c(pc1,pc2,pc3)
                overlap <- length( intersect(pcards, dcards) )
                
                if(overlap==0) {
                  prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                    ( (4*1*1) * factorial(3) * (per.dragon.straight.c1) ) / totalper
                } else if (overlap==1) {
                  # If 1 rank overlap, means 1 suit overlap
                  prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                    ( (3*1*1) * factorial(3) * (per.dragon.straight.c1) ) / totalper
                } else if (overlap==2) {
                  if(is.element(c1,pcards) & is.element(c2,pcards)) {
                    # Case Dragon 2 suits, 1 suit overlap
                    prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                      ( (3*1*1) * factorial(3) * (per.dragon.straight.c1.suits.11x) ) / totalper
                  }
                  if(is.element(c1,pcards) & is.element(c3,pcards)) {
                    # Case Dragon 2 suits, 1 suit overlap
                    prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                      ( (3*1*1) * factorial(3) * (per.dragon.straight.c1.suits.1x1) ) / totalper
                  }
                  if(is.element(c2,pcards) & is.element(c3,pcards)) {
                    # Case Dragon 2 suits, 1 suit overlap
                    prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                      ( (3*1*1) * factorial(3) * (per.dragon.straight.c1.suits.x11) ) / totalper
                  }
                  
                  # Case Dragon 3 suits, thus 2 suit overlap
                  prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                    ( (2*1*1) * factorial(3) * (per.dragon.straight.c1.suits.123) ) / totalper
                } else if (overlap==3) {
                  # Case Dragon 2 suits, thus 2 suit overlap
                  prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                    ( (2*1*1) * factorial(3) * (per.dragon.straight.c1.suits.11x + per.dragon.straight.c1.suits.1x1 + per.dragon.straight.c1.suits.x11) ) / totalper
                  
                  # Case Dragon 3 suits, thus 3 suit overlap
                  prob.phoenix.higher.flush <- prob.phoenix.higher.flush +
                    ( (1*1*1) * factorial(3) * (per.dragon.straight.c1.suits.123) ) / totalper
                }
              }
            }
          }
        }
      }
    }
  }
  
  stopifnot( abs(prob.dragon.straight - prob.tmp) < 0.000000001 )
  prob.dragon.win.straight <- prob.dragon.straight - 
    prob.phoenix.higher.3OAK - prob.phoenix.higher.straight.sf - prob.phoenix.higher.flush - prob.phoenix.tie.straight
  
  # Due to symmetry of Dragon/Phoenix, the probability of Phoenix winning with SF is the same.
  # Thus the probability of any Dragon/Phoenix winning with SF
  prob.win.straight <- prob.dragon.win.straight * 2
  prob.win.straight
  
  return(prob.win.straight)
}

get.prob.winner.pair <- function(PP=2)
{
  # Probability of dragon pair PP-Ace (14-PP+1 ranks)
  prob.dragon.pair9toA <- ( (14-PP+1) * (4*3/2)*48 * factorial(3) *(n-3)*(n-4)*(n-5) ) / totalper
  prob.dragon.draw.pair9toA <- 0
  
  # Permutations of Phoenix with higher normal flush and all straight flush
  prob.phoenix.lower.pair   <- 0
  prob.phoenix.lower.hicard <- 0

  prob.tmp <- 0
  
  for(paircard in c(PP:14))
  {
    c1 <- paircard
    c2 <- paircard
    
    for(c3 in c(2:14))
    {
      # No triples
      if(c1==c3) next
      dcards <- c(c1,c2,c3)
      
      per.dragon.pair.c1 <- ( (4*3/2)*4 * factorial(3) )
      per.dragon.pair.c1.suits.121 <- ( (4*3/2)*2 * factorial(3) ) # Same suit of 3rd card with pair cards
      per.dragon.pair.c1.suits.123 <- ( (4*3/2)*2 * factorial(3) ) # Different suit of 3rd card with pair cards

      prob.tmp <- prob.tmp + ( per.dragon.pair.c1 * (n-3)*(n-4)*(n-5) ) / totalper
      
      # No need to care about 1-2-3 straight, as it is higher than Dragon Pair
      for(pc1 in c(2:14))
      {
        for(pc2 in c(pc1:14))
        {
          for(pc3 in c(pc2:14))
          {
            pcards <- c(pc1,pc2,pc3)
            
            flag.phoenix.unique          <- (pc1!=pc2 & pc2!=pc3 & pc1!=pc3)
            # Straight & SF
            flag.phoenix.straight        <- (pc2==pc1+1 & pc3==pc2+1)
            flag.phoenix.triple          <- (pc1==pc2 & pc2==pc3)
            flag.phoenix.pair            <- !flag.phoenix.unique & !flag.phoenix.triple
            # For case of normal flush & hicard
            flag.phoenix.hicard          <- ( flag.phoenix.unique & !flag.phoenix.straight )
            
            # Triple, SF, Straight beats pair. Normal Flush will be handled below.
            if(flag.phoenix.triple | flag.phoenix.straight) next
            
            # Case of Phoenix lower pair
            if(flag.phoenix.pair ) {
              # Pair value (just p2)
              value.phn.pair    <- pc2
              value.phn.3rdcard <- pc1*(pc1!=pc2) + pc3*(pc2!=pc3)
              # If Phoenix pair is higher, ignore
              if(value.phn.pair > c1) next
              
              # How many unique cards to form Phoenix pair, 3 if coincide with 3rd Dragon card, 2 if same pair with dragon pair
              a <- 4 - 1*(value.phn.pair==c3) - 2*(value.phn.pair==c1)
              # How many unique cards to form Phoenix 3rd card
              c <- 4 - 1*(pc3==c3) - 2*(pc3==c1)
              stopifnot(a>0 & c>0)
              
              # Dragon & Phoenix pair equal
              if(value.phn.pair==c1) {
                # 3rd card is equal, so tie
                if(c3==value.phn.3rdcard) {
                  prob.dragon.draw.pair9toA <- prob.dragon.draw.pair9toA +
                    ( (a*(a-1)/2)*c * factorial(3) * per.dragon.pair.c1 / totalper )
                # 3rd card lower, so Phoenix loses
                } else if(c3>value.phn.3rdcard) {
                  prob.phoenix.lower.pair <- prob.phoenix.lower.pair +
                    ( (a*(a-1)/2)*c * factorial(3) * per.dragon.pair.c1 / totalper )
                }
              }
              else if(value.phn.pair<c1) {
                prob.phoenix.lower.pair <- prob.phoenix.lower.pair +
                  ( (a*(a-1)/2)*c * factorial(3) * per.dragon.pair.c1 / totalper )
              }
            }
            else if (flag.phoenix.hicard) {
              rankoverlap   <- intersect(dcards, pcards)
              n.rankoverlap <- length(rankoverlap)
              
              # How many cards of rank "pc1" left
              a <- 4 - 2*(pc1==c1) - 1*(pc1==c3)
              b <- 4 - 2*(pc2==c1) - 1*(pc2==c3)
              c <- 4 - 2*(pc3==c1) - 1*(pc3==c3)
              
              # Permutations of normal flush
              # No rank overlap
              if(n.rankoverlap==0) {
                # Permutatons of hicard (no flush)
                prob.phoenix.lower.hicard <- prob.phoenix.lower.hicard +
                  ( (a*b*c - 4*1*1) * factorial(3) * per.dragon.pair.c1 ) / totalper
              } else if (n.rankoverlap==1) {
                # Overlap with Dragon pair card
                if(is.element(c1, rankoverlap)) {
                  # This means 2 suits left only for the flush
                  prob.phoenix.lower.hicard <- prob.phoenix.lower.hicard +
                    ( (a*b*c - 2*1*1) * factorial(3) * per.dragon.pair.c1 ) / totalper
                } else {
                  # Overlap with Dragon 3rd card
                  # This means 3 suits left only for the flush
                  prob.phoenix.lower.hicard <- prob.phoenix.lower.hicard +
                    ( (a*b*c - 3*1*1) * factorial(3) * per.dragon.pair.c1 ) / totalper
                }
              } else if (n.rankoverlap==2) {
                # Case Dragon 3rd card is same suit with Dragon pair cards
                # This means 2 suits left only for the flush
                prob.phoenix.lower.hicard <- prob.phoenix.lower.hicard +
                  ( (a*b*c - 2*1*1) * factorial(3) * per.dragon.pair.c1.suits.121 ) / totalper

                # Case Dragon 3rd card is different suit with Dragon pair cards
                # This means 1 suits left only for the flush
                prob.phoenix.lower.hicard <- prob.phoenix.lower.hicard +
                  ( (a*b*c - 1*1*1) * factorial(3) * per.dragon.pair.c1.suits.123 ) / totalper
              }
            }
          }
        }
      }
    }
  }

  stopifnot( abs(prob.dragon.pair9toA - prob.tmp) < 0.000000001 )
  prob.dragon.win.pair9toA <- prob.phoenix.lower.pair + prob.phoenix.lower.hicard

  # Due to symmetry of Dragon/Phoenix, the probability of Phoenix winning with SF is the same.
  # Thus the probability of any Dragon/Phoenix winning with SF
  prob.win.pair9toA <- prob.dragon.win.pair9toA * 2
  prob.win.pair9toA
  
  return(prob.win.pair9toA)
}

get.prob.zhajinhua.tie <- function()
{
  prob.zhajinhua.tie <- 0
  
  for(c1 in c(1:14))
  {
    for(c2 in c(c1:14))
    {
      for(c3 in c(c2:14))
      {
        # Value of 1 only used for smallest straight 1-2-3
        if(c1==1 & !(c2==2 & c3==3)) next
        
        dcards <- c(c1,c2,c3)
        
        flag.dragon.unique          <- (c1!=c2 & c2!=c3 & c1!=c3)
        # Straight & SF
        flag.dragon.straight        <- (c2==c1+1 & c3==c2+1)
        flag.dragon.triple          <- (c1==c2 & c2==c3)
        flag.dragon.pair            <- !flag.dragon.unique & !flag.dragon.triple
        # For case of normal flush & hicard
        flag.dragon.hicard          <- ( flag.dragon.unique & !flag.dragon.straight )
        
        for(pc1 in c(1:14))
        {
          for(pc2 in c(pc1:14))
          {
            for(pc3 in c(pc2:14))
            {
              # Value of 1 only used for smallest straight 1-2-3
              if(pc1==1 & !(pc2==2 & pc3==3)) next
              
              pcards <- c(pc1,pc2,pc3)
              
              flag.phoenix.unique          <- (pc1!=pc2 & pc2!=pc3 & pc1!=pc3)
              # Straight & SF
              flag.phoenix.straight        <- (pc2==pc1+1 & pc3==pc2+1)
              flag.phoenix.triple          <- (pc1==pc2 & pc2==pc3)
              flag.phoenix.pair            <- !flag.phoenix.unique & !flag.phoenix.triple
              # For case of normal flush & hicard
              flag.phoenix.hicard          <- ( flag.phoenix.unique & !flag.phoenix.straight )
              
              # Triples cannot tie
              if(flag.dragon.triple | flag.phoenix.triple) next
              
              # SF Tie
              if(flag.dragon.straight & flag.phoenix.straight)
              {
                prob.zhajinhua.tie <- prob.zhajinhua.tie +
                  ( 4*1*1 * factorial(3) * 3*1*1 * factorial(3) ) / totalper
              }
              
              # Flush Tie
              if(flag.dragon.hicard & flag.phoenix.hicard & c1==pc1 & c2==pc2 & c3==pc3)
              {
                prob.zhajinhua.tie <- prob.zhajinhua.tie +
                  ( 4*1*1 * factorial(3) * 3*1*1 * factorial(3) ) / totalper
              }
                
              # Straight Tie
              if(flag.dragon.straight & flag.phoenix.straight & c1==pc1)
              {
                prob.zhajinhua.tie <- prob.zhajinhua.tie +
                  ( ( 4*4*4 - 4*1*1 ) * factorial(3) ) * ( (3*3*3 - 3*1*1) * factorial(3) ) / totalper
              }
              
              # Pair Tie
              if(flag.dragon.pair & flag.phoenix.pair & c1==pc1 & c2==pc2 & c3==pc3)
              {
                prob.zhajinhua.tie <- prob.zhajinhua.tie +
                  ( (4*3/2)*2 * factorial(3) * (2*1/2)*1 * factorial(3) ) / totalper
              }
              
              # Hicard Tie
              if(flag.dragon.hicard & flag.phoenix.hicard & c1==pc1 & c2==pc2 & c3==pc3)
              {
                prob.zhajinhua.tie <- prob.zhajinhua.tie +
                  ( ( 4*4*4 - 4*1*1 ) * factorial(3) ) * ( (3*3*3 - 3*1*1) * factorial(3) ) / totalper
              }
            }
          }
        }
      }
    }
  }
  
  return(prob.zhajinhua.tie)
}

get.prob.winner.hicard <- function()
{
  return(1 - get.prob.winner.3ofakind() - get.prob.winner.straightflush() -
           get.prob.winner.flush() - get.prob.winner.straight() -
           get.prob.winner.pair(2) - get.prob.zhajinhua.tie())
}

get.prob.winner.3ofakind()
get.prob.winner.straightflush()
get.prob.winner.flush()
get.prob.winner.straight()
get.prob.winner.pair(2)
get.prob.winner.pair(9)
get.prob.winner.hicard()
get.prob.zhajinhua.tie()

