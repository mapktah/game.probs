
#
# Given say k=c(0,2,3), n=5
# Return c(1, 5*4=20, 5*4*3=60)
#
calculate.total.permutations <- function(k, n)
{
  tp <- rep(1, times=length(k))
  max <- max(k)
  for(i in c(1:max))
  {
    # If cell is greater than 0 multiply with tp, other wise by 1
    tp <- ( ( n*(k > 0) ) + 1*(k<=0) ) * tp
    n <- n - 1
    k <- k - 1
  }
  return(tp)
}

#
# Calculate permutations in matrix m. <n> is the total elements in the same dimension as m.
#
#m=matrix(data=c(4,3,1, 4,0,2, 2,2,0, 1,4,3), nrow=3, ncol=4)
#n=matrix(data=c(rep(4,times=9), rep(16, times=3)), nrow=3, ncol=4)
calculate.permutations <- function(m, n)
{
  nrow <- dim(m)[1]
  ncol <- dim(m)[2]
  if(dim(n)[1] != nrow | dim(n)[2] != ncol)
    stop("Both m and n must have same dimensions!")
  mcombs <- matrix(data=1, nrow=dim(m)[1], ncol=dim(m)[2])
  
  if(sum(m>n) != 0)
    stop(paste("Inconsistent, cannot choose more object than available m=",m, ", n=", n, sep=""))
  
  while(sum(m) > 0)
  {
    # Done with m, all have been reduced to 0
    if(sum(m) == 0) break
    
    # Multiply by 1 for 0 elements, otherwise n
    mcombs <- ( ( (m>0) * n ) + ( (m<=0) * 1 ) ) * mcombs
    m <- m-1
    m[m<0] <- 0
    n <- n-1
    n[n<0] <- 0
  }
  
  # Now multiply all columns to get combinations of each row
  rowcombs <- rep(1,nrow)
  for(i in c(1:ncol))
    rowcombs <- rowcombs * mcombs[,i]
  
  return(rowcombs)
}

#
# Returns how many cards with value 10
# When nranks=10, 0 is used to represent 10, J, Q, K
# When nranks=13, 0 is used to represent Ace, 1 to represent 2, ... 10 to represent J, 11 Q, 12 K
#
zerorankcount <- function(nranks)
{
  cnt <- 4
  # If we combine 10 to King, then there are 16 cards of value 0
  if(nranks==10) cnt <- 16
  return(cnt)
}

check.card.frequencies.valid <- function(nranks, ndecks, freq)
{
  base.digits <- c(0:(nranks-1))
  
  # Invalid if occurrence exceeds number in deck
  valid <- rep(TRUE, dim(freq)[1])
  for(i in base.digits) {
    rankcount <- 4
    
    # If we combine 10 to King, then there are 16 cards of value 0
    if(i==0) rankcount <- zerorankcount(nranks)
    
    valid <- valid & (freq[,i+1] <= rankcount*ndecks)
  }
  
  return(valid)
}

get.card.frequencies <- function(ncards, nranks, card.permutations)
{
  # Form a table of <ncards> columns (cards)
  factor <- nranks
  base.digits <- c(0:(factor-1))
  
  #
  # Count frequencies of card ranks
  #
  freq <- data.frame()
  for(i in base.digits)
  {
    x <- as.matrix((card.permutations==i)*1)
    
    if(i==0) { freq <- as.data.frame(rowSums(x)); colnames(freq) <- c("Freq.0") }
    else { freq[,paste("Freq.",i,sep="")] <- rowSums(x) }
  }
  
  # Sanity Check (every row must have same sum = ncards)
  freq.sum <- rowSums(freq)
  if( (min(freq.sum) != max(freq.sum)) | min(freq.sum) != ncards )
    stop(paste("Error min(freq)=",min(freq.sum),", max(freq)=",max(freq.sum),sep=""))
  
  return(freq)
}

#
# All possible layout of <ncards> giveout, without calculation of the number of permutations.
# Deck consists of <nranks> ranks (if 13, then Ace to King,
# if 10 we group all 10 value cards together)
# Useful for calculation of Baccarat probabilities, but not Blackjack/Poker (too many combinations)
#
get.all.card.permutations.basic <- function(ncards, nranks, ndecks)
{
  # nranks can only be 10 (from Ace to 10, J-K counted as 10 card rank) or 13 (from Ace to King)
  if( !(nranks==10 | nranks==13) & ncards > 0 )
    stop( paste("nranks must be 10 or 13! ncards must be greater than 0! Given nranks=",nranks,", ncards=",ncards,sep=) )
  
  # Form a table of <ncards> columns (cards)
  factor <- nranks
  base.digits <- c(0:(factor-1))
  
  # Create the 0th power
  card.permutations <- data.frame(Card.1=base.digits)
  for(i in c(1:(ncards-1)))
  {
    if(ncards <= 1) break
    
    # Create the next "power" of "factor"
    b2 <- rep(base.digits, factor**i)
    # Sort to get numbers in order {000000....111111......222222....}
    b2 <- sort(b2)
    
    # Create a repeat of the dataframe <factor> times
    tmp <- card.permutations
    # Similar to the rep() command for dataframes, repeat the previous dataframe <factor> times
    for(j in c(2:factor)) tmp <- rbind(tmp, card.permutations)
    
    card.permutations <- cbind(tmp, b2)
  }
  
  colnames(card.permutations) <- paste("Card.",c(1:ncards),sep="")
  
  # Sanity check (The cards that appear in each line must reflect line number minus 1)
  lineno <- c(0:(factor**ncards - 1))
  # The line number must be the same as the implied line number
  implied.lineno <- card.permutations[,1]
  if(ncards > 1) {
    for(i in c(2:ncards))
      implied.lineno <- implied.lineno + ( card.permutations[,i] * (factor**(i-1)) )
  }
  if(!all.equal(lineno, implied.lineno))
    stop(paste("Error in generation"), sep="")
  
  return(card.permutations)
}

get.card.permutations <- function(ncards, nranks, ndecks, freq, no.card.choices)
{
  perms.part <- ( freq>-999999 ) * 1
  for(i in c(0:(ncards-1)))
  {
    # First get number of ways in this loop
    no.of.ways <- (no.card.choices - i) * (freq - i > 0)
    # Replace 0 with 1
    no.of.ways[no.of.ways==0]<-1
    perms.part <- perms.part * no.of.ways * (freq <= no.card.choices)
  }
  perms <- perms.part[,1]
  for(i in c(2:(dim(perms.part)[2])))
  {
    perms <- perms * perms.part[,i]
  }

  return(perms)
}

#
# All possible layout of <ncards> giveout.
# Deck consists of <nranks> ranks (if 13, then Ace to King,
# if 10 we group all 10 value cards together)
# Useful for calculation of Baccarat probabilities, but not Blackjack/Poker (too many combinations)
#
get.all.card.permutations <- function(ncards, nranks, ndecks)
{
  card.permutations <- get.all.card.permutations.basic(ncards = ncards, nranks = nranks, ndecks = ndecks)

  # Count frequencies of card ranks
  freq <- get.card.frequencies(ncards = ncards,nranks = nranks, card.permutations = card.permutations)

  card.permutations$Valid <- check.card.frequencies.valid(nranks = nranks, ndecks = ndecks, freq = freq)
  
  # Clone the freq dataframe
  no.card.choices <- ( freq>-999999 ) * (ndecks*4)  # For each card, there is ndecks*4 of them
  no.card.choices[,1] <- ndecks * zerorankcount(nranks)

  card.permutations$Combinations <- get.card.permutations(ncards = ncards, nranks = nranks, ndecks = ndecks,
                                                          freq = freq, no.card.choices = no.card.choices)

  # Sanity Check
  total.combs <- 1
  for(i in c(0:(ncards-1))) {
    total.combs <- total.combs * (ndecks*52 - i)
  }
  cal.combs <- sum(card.permutations$Combinations[card.permutations$Valid==TRUE])
  if(cal.combs != total.combs )
    stop(paste("Combinations expected = ", total.combs, " not equal to calculated combinations ", cal.combs, sep=""))
  
  return(card.permutations)
}

#
# All possible combinations (order or permutation not important) of <ncards> giveout.
# Deck consists of <nranks> ranks (if 13, then Ace to King,
# if 10 we group all 10 value cards together)
# Useful for calculation of more complicated games with more than 5-6 cards like Baccarat.
#
get.all.card.combinations <- function(ncards, nranks, ndecks)
{
  card.permutations <- get.all.card.permutations.basic(ncards = ncards, nranks = nranks, ndecks = ndecks)
  
  # Count frequencies of card ranks
  freq <- get.card.frequencies(ncards = ncards,nranks = nranks, card.permutations = card.permutations)
  
  card.combinations <- freq
  card.combinations$Valid <- check.card.frequencies.valid(nranks = nranks, ndecks = ndecks, freq = freq)
  
  # Clone the freq dataframe
  no.card.choices <- ( freq>-999999 ) * (ndecks*4)  # For each card, there is ndecks*4 of them
  no.card.choices[,1] <- ndecks * zerorankcount(nranks)

  card.combinations$Permutations <- get.card.permutations(ncards = ncards, nranks = nranks, ndecks = ndecks,
                                             freq = freq, no.card.choices = no.card.choices)
  
  x <- card.combinations
  agg <- aggregate(x = data.frame(Permutations = card.combinations$Permutations),
                   by = list(Freq.0=x$Freq.0, Freq.1=x$Freq.1, Freq.2=x$Freq.2, Freq.3=x$Freq.3, Freq.4=x$Freq.4,
                             Freq.5=x$Freq.5, Freq.6=x$Freq.6, Freq.7=x$Freq.7, Freq.8=x$Freq.8, Freq.9=x$Freq.9,
                             Freq.10=x$Freq.10, Freq.11=x$Freq.11, Freq.12=x$Freq.12, Valid=x$Valid),
                   FUN = "sum")
  return(agg)
}
#
# Utility Functions for Card Games
#

get.simple.deck <- function(ndecks, facecard=0)
{
  # Simplified deck model without caring about card suit
  deck <- data.frame(Points=c(1:9,facecard), TotalCards=ndecks*c(rep(4,9),16))
  return(deck)
}

get.full.deck <- function(ndecks)
{
  # Full deck, 1-13 as Ace to King, A-D as Piki, Chervi, Trefi, Bubni. 
  decka <- merge(c(1:13), c("Piki", "Chervi", "Trefi", "Bubni"))
  colnames(decka) <- c("Card", "House")
  decka$Points <- pmin(10, decka$Card)
  decka <- decka[order(decka$Card), ]
  decka$TotalCards <- ndecks*1
  return(decka)
}

# Utility function to retrieve card count in deck
get.card.count.in.deck <- function(deck, point) { return(deck[deck$Points==point, "TotalCards"]) }

#
# Utility function to make code cleaner
#
reduce.deck <- function(deck, point, reduce.by)
{
  # Can't reduce by more than what is available
  if(as.numeric(deck$TotalCards[deck$Points==point]) < reduce.by) return(data.frame())
  
  deck[deck$Points==point, "TotalCards"] <- as.numeric( deck[deck$Points==point, "TotalCards"] - reduce.by )
  return(deck)
}

get.probability.cardsetpermutation <- function(deck, cards)
{
  if(length(cards)==0) return(0)
    
  prob <- 1
  for(card in cards)
  {
    prob <- prob * deck$TotalCards[deck$Points==card] / sum(deck$TotalCards)
    deck <- reduce.deck(deck=deck, point=card, reduce.by=1)
  }
  return(prob)
}

#
# Given a deck <df.deck>, we draw <draw> number of cards, with sum <sum>
#
get.combination.count.of.sum <- function(deck, draw, sum, verbose=FALSE)
{
  if(sum<0 | sum>9) return(0)
  
  if(draw == 1)
  {
    # Card drawn must be equal to sum
    return(get.card.count.in.deck(deck, sum))
  }
  
  count <- 0
  
  for(card1 in c(0:9))
  {
    deck.tmp <- deck
    
    # Number of ways to get first card
    n.card1 <-get.card.count.in.deck(deck.tmp, card1)
    
    # Reduce the deck for remaining card picks
    deck.tmp <- reduce.deck(deck.tmp, point=card1, reduce.by=1)
    
    next.sum <- (10+sum-card1)%%10
    count.tmp <- n.card1 * get.combination.count.of.sum(deck.tmp, draw-1, next.sum, verbose=FALSE)
    count <- count + count.tmp
    
    if(verbose) {
      print(paste("Card 1 Value = ", card1, " next sum = ", next.sum,
                  ", Combinations = ", count.tmp, sep=""))
    }
  }
  
  return(count)
}

get.combination.count.of.sum.cum <- function(deck, draw, sum)
{
  count <- 0
  
  for(s in c(0:sum))
    count <- count + get.combination.count.of.sum(deck, draw, s)
  
  return(count)
}
