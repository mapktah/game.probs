rm(list=ls())

# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)

# Length of sequence
N = 13
# Length of streak
S = 8
# Win
p = 0.458597
# Loss
q = 1-p

get.combination <- function(n, m)
{
  totcom <- 1
  if(m<=0 | m>=n) return(1)
  
  for(i in c(1:m))
  {
    totcom <- totcom * (n-i+1)
  }
  
  return(totcom / factorial(m))
}

total.combinations <- 0
max.streaks <- floor(N/S)
total.streaks <- rep(0, times=max.streaks)

# If length is N, there can be as many as C(n-1,n-1) roads
max.roads <- N
for(nroads in c(1:N))
{
  print(paste(nroads, " roads..", sep=""))
  
  # Ways to choose the different road cuts
  total.comb.roadcuts <- get.combination(N-1,nroads-1)
  
  # The actual cut combinations
  cut.combinations <- combn(N-1,nroads-1)
  cut.combinations <- t(cut.combinations)
  
  # Case only 1 road (means no cut)
  roads <- data.frame(X=N)
  # Case more than 1 road (have cuts)
  if(dim(cut.combinations)[2]>0)
    roads <- data.frame(cut.combinations,X=N)
  
  ncols <- dim(roads)[2]
  colnames(roads) <- c(1:ncols)
  for(col in c(ncols:2))
  {
    if(ncols<2) break
    roads[,col] <- roads[,col] - roads[,(col-1)]
  }
  
  # Count streaks (case start with win, and case start with loss)
  for(mod in c(0:1))
  {
    chosen.columns <- ( c(1:ncols) + mod )%%2 == 0
    roads.win <- as.data.frame( roads[,chosen.columns] )
    if(dim(roads.win)[2]==0) next
    
    # Count streaks in each road
    streaks <- floor( roads.win / S )
    streaks <- rowSums(streaks)
    
    for(j in c(1:max.streaks))
      total.streaks[j] <- total.streaks[j] + sum( streaks[streaks==j] )
  }
}

total.streaks

