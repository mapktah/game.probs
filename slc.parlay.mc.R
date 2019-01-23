#
# Monte-Carlo test on formulas
#

df <- data.frame()

# Number of simulations
SIMS <- 1000000
DRAW <- 27
N <- 100

# Generate the random numbers first
rnums <- floor(runif(SIMS*DRAW, min=0, max=100)) + 1

# For 2D Parlay-2 or Parlay-3, count hits of chosen numbers 24, 9, 92, 19
v1 <- rnums
v1[!(v1==24)] <- 0
v1[v1==24] <- 1

v2 <- rnums
v2[!(v2==9) ] <- 0
v2[v2==9] <- 1

v3 <- rnums
v3[!(v3==92) ] <- 0
v3[v3==92] <- 1

v4 <- rnums
v4[!(v4==19) ] <- 0
v4[v4==19] <- 1

# Create a matrix of dimension (SIMS,DRAW)
m1 <- matrix(data=v1, nrow=SIMS, ncol=DRAW)
m2 <- matrix(data=v2, nrow=SIMS, ncol=DRAW)
m3 <- matrix(data=v3, nrow=SIMS, ncol=DRAW)
m4 <- matrix(data=v4, nrow=SIMS, ncol=DRAW)

# Calculate the hits of ball 1 and ball 2
hit1 <- rowSums(m1)
hit2 <- rowSums(m2)
hit3 <- rowSums(m3)
hit4 <- rowSums(m4)

# Get minimum hit for Parlay-2 and Parlay-3
hitmin.parlay2 <- pmin(hit1, hit2)
hitmin.parlay3 <- pmin(hit1, hit2, hit3)
hitmin.parlay4 <- pmin(hit1, hit2, hit3, hit4)

#odds2 <- 650/56
#odds3 <- 3500/81

# Expected Value
expval2 <- sum(hitmin.parlay2) / SIMS
expval3 <- sum(hitmin.parlay3) / SIMS
expval4 <- sum(hitmin.parlay4) / SIMS

