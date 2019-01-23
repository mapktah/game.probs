
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

ndecks = 1
ncards = 3

#
# Calculate total combinations of straight flush
#
poker.get.combinations.sf <- function(ndecks, ncards)
{
  # Calculate for one case first: {1,2,3..}
  # 4*ndecks ways to choose Ace, then after that ndecks ways to choose 2, ndecks ways to choose 3,...
  combs <- 4 * (ndecks**ncards)
  # Flush can happen from {A,2,3..}, {2,3,4,..}, .. {...Q,K,A}, thus Ace to (14-ncards+1)
  combs <- combs * (14-ncards+1)
  return(combs)
}