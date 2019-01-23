
#
# Probability of a number occuring exactly <n> times in <totaldraw> independent draws.
# <n> can take values from 1 to <totalnumbers>.
# So to know the probability of the number 22 occurring exactly 3 times from an independent
# draw of 27 balls (numbered randomly uniformly distributed from 1 to 100), call
# prob.occur(3, 27, 100)
#
prob.occur <- function(n,totaldraw,totalnumbers)
{
  if(totaldraw==0) return(0)
  
	return( (factorial(totaldraw)/(factorial(totaldraw-n)*factorial(n))) * 
			((1/totalnumbers)**n) * 
			(((totalnumbers-1)/totalnumbers)**(totaldraw-n)) )
}

#
# Probability of a chosen number occurring n or more times.
#
prob.occur.cum <- function(n,totaldraw,totalnumbers) {
	p=0

	for(i in c(n:totaldraw)) {
		p = p + prob.occur(n=i,totaldraw,totalnumbers)
	}
	
	return(p)
}

prob.parlay.any <- function(hit,D,N,parlay)
{
  pcum = 0
  
  # 1 exact hit, 2 exact hits, ... , <parlay> exact hits
  for(i in c(1:parlay))
  {
    # Probability of i exact hit
    p.exact.hit = 1
    D.next = D
    N.next = N
    for(ee in c(1:i)) {
      p.exact.hit = p.exact.hit * prob.occur(hit, D.next, N.next)
      D.next = D.next - hit
      N.next = N.next - 1
    }
    # print(paste("Iter ",i,": p.exact.hit = ",p.exact.hit,sep=""))
    
    if(i < parlay) {
      # Probability of remaining (parlay-i) balls hit (hit+1) times and above
      p.rem.hitplus1 = 0
      frmhit <- hit+1
      tohit <- floor( (D-(i*hit)) / i )
      for(hh in c(frmhit:tohit))
        p.rem.hitplus1 = p.rem.hitplus1 + prob.parlay(hh, D-(i*hit), N-i, parlay-i)
      
      # i of the numbers hit exactly <hit> times
      pcum = pcum +
        (
          ( factorial(parlay) / ( factorial(parlay - i) * factorial(i) ) ) *
            p.exact.hit * p.rem.hitplus1    )
    } else {
      pcum = pcum + p.exact.hit
    }
    # print(paste("Iter ",i,": pcum = ",pcum,sep=""))
  }
  
  return(pcum)
}

#
# Probability that <parlay> chosen numbers (different numbers), hits <hit> times (minimum hit of
# the numbers is <hit>)
#
prob.parlay <- function(hit,D,N,parlay)
{
  if(D<=0 | parlay*hit>=D) return(0);
    
  pcum = 0
  
  if(parlay==0) {
    pcum = 1
  }
  else if(parlay==1) {
    pcum = prob.occur(hit, D, N)
  }
  else if(parlay==2) {
    # Case of both exact hit, and one of them exact hit (multiply by 2 because 2 cases of this)
	  pcum = prob.occur(hit, D, N) * prob.occur(hit, D-hit, N-1) +
		  2 * prob.occur(hit, D, N) * prob.occur.cum(hit+1, D-hit, N-1)
  }
  else if(parlay == 3) {
    pcum = 0
    # Probability of all 3 hit exactly <hit> times
    pcum = pcum + 
              1 * prob.occur(hit,D,N) * prob.occur(hit,D-hit,N-1) * prob.occur(hit,D-2*hit,N-2)
    # Probability of 2 numbers hit exactly <hit> times, while another hits greater than <hit> times.
    # 3 possible combinations.
    pcum = pcum + 
              3 * prob.occur(hit,D,N) * prob.occur(hit,D-hit,N-1) * prob.occur.cum(hit+1,D-2*hit,N-2)
    
    # Probability of 1 ball hit exactly <hit> times, while the other 2 bot hit greater than <hit> times.
    # Sum up probability of parlay 2 hit <hit+1> times, hit <hit+2> times, ...
    p.rem.hitplus1 = 0
    frmhit <- hit+1
    tohit <- D-hit
    for(hh in c(frmhit:tohit))
      p.rem.hitplus1 = p.rem.hitplus1 + prob.parlay(hh, D-hit, N-1, 2)
    # 3 Combinations for this
    pcum = pcum +
              3 * prob.occur(hit,D,N) * p.rem.hitplus1
  }
  else if(parlay>=4)
  {
    # 1 exact hit, 2 exact hits, ... , <parlay> exact hits
    for(i in c(1:parlay))
    {
      # Probability of i exact hit
      p.exact.hit = 1
      D.next = D
      N.next = N
      for(ee in c(1:i)) {
        p.exact.hit = p.exact.hit * prob.occur(hit, D.next, N.next)
        D.next = D.next - hit
        N.next = N.next - 1
      }
      # print(paste("Iter ",i,": p.exact.hit = ",p.exact.hit,sep=""))
      
      if(i < parlay) {
        # Probability of remaining (parlay-i) balls hit (hit+1) times and above
        p.rem.hitplus1 = 0
        frmhit <- hit+1
        tohit <- floor( (D-(i*hit)) / i )
        for(hh in c(frmhit:tohit))
          p.rem.hitplus1 = p.rem.hitplus1 + prob.parlay(hh, D-(i*hit), N-i, parlay-i)
        
        # i of the numbers hit exactly <hit> times
        pcum = pcum +
          (
            ( factorial(parlay) / ( factorial(parlay - i) * factorial(i) ) ) *
              p.exact.hit * p.rem.hitplus1    )
      } else {
        pcum = pcum + p.exact.hit
      }
      # print(paste("Iter ",i,": pcum = ",pcum,sep=""))
    }
  }

	return(pcum)
}

prob.parlay.all <- function(D,N,parlay) {
	pcum = 0

	for(i in c(1:D)) {
		p = prob.parlay(i,D,N,parlay)
		if(is.na(p)) break
		pcum = pcum + p
	}

	return(pcum)
}

#
# From chosen <parlay> balls, hit exactly <parlay.hit> balls <hit> times
#
prob.parlay.partialhit <- function(hit, D, N, parlay, parlay.hit)
{
  if(D<=0 | parlay*hit>=D) return(0);
  
  # Non successful balls, totally no hits or appearance
  nsb <- parlay - parlay.hit

  pcum = ( factorial(parlay) / ( factorial(parlay.hit) * factorial(nsb) ) )* 
              prob.parlay(hit, D, N-nsb, parlay.hit)

  return(pcum)
}

prob.parlay(1,27,100,2)
prob.parlay.any(1,27,100,2)
prob.parlay(1,27,100,3)
prob.parlay.any(1,27,100,3)

prob.parlay.all(27,100,1)
prob.parlay.all(27,100,2)
prob.parlay.all(27,100,3)
prob.parlay.all(27,100,4)
#prob.parlay.all(27,100,5)
