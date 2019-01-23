# The reason this file is separated from the main file is so that the main R file is the same
# on all computers, only this file is unique to the user.
rm(list=ls())

WORKING.DIR <- "~/dev/util"
# Change directory
setwd(WORKING.DIR)

# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)

#
# Choose your product group
#
prdgrp <- "SB"

feed.raw <- NA
if(prdgrp == "RNG") {
  feed.raw <- read.csv("data/RNG RB AprMayJun17.csv", header=TRUE)
} else if(prdgrp == "LD") {
  feed.raw <- read.csv("data/live dealer RB AprMayJun17.csv", header=TRUE)
} else if(prdgrp == "SB") {
  feed.raw <- read.csv("data/sportsbook AprMayJun17.csv", header=TRUE)
}

feed <- feed.raw

if(prdgrp == "RNG")
{
  # For some reason the SQL exported as factor, not even character or numeric
  feed$RTP[feed$RTP == "NULL"] <- NA
  feed$RTP <- as.numeric(as.character(feed$RTP))
  
  # Remove those with NA RTP (only for RNG)
  feed.na.removed <- feed[!is.na(feed$RTP),]
  
  feed.na.removed$Margin <- 1 - feed.na.removed$RTP
  
  feed <- feed.na.removed
  
  # Center of mass for margin
  margin.com <- sum(feed.na.removed$Margin * feed.na.removed$Totalbetamount) / sum(feed.na.removed$Totalbetamount)
  
} else if(prdgrp == "LD") {
  # Get only top games
  agg.feed <- aggregate(x=feed[,c("Totalbetamount", "Betcount")], by=list(GameName=feed$GameName), FUN="sum")
  agg.feed <- agg.feed[order(agg.feed$Totalbetamount, decreasing = TRUE), ]
  agg.feed$TurnoverProportion <- agg.feed$Totalbetamount / sum(agg.feed$Totalbetamount)
  agg.feed$TurnoverProportionCumulative <- rep(0,times=dim(agg.feed)[1])
  prop.cum <- 0
  for(i in c(1:dim(agg.feed)[1])) {
    prop.cum <- prop.cum + agg.feed$TurnoverProportion[i]
    agg.feed$TurnoverProportionCumulative[i] <- prop.cum
  }
  
  # Take top 10 games only
  topgames <- agg.feed$GameName[1:10]
  feed <- feed[is.element(feed$GameName, topgames), ]
  
} else if(prdgrp == "SB") {
  # Standardize to decimal odds
  feed$DecimalOdds <- 
    (feed$OddsType == "Decimal Odds") * feed$odds +
    (feed$OddsType == "HK Odds")      * ( 1 + feed$odds ) +
    (feed$OddsType == "Malay Odds")   * ( 1 + ((feed$odds >= 0) * feed$odds) + ((feed$odds < 0) * 1/abs(feed$odds)) ) +
    (feed$OddsType == "Indo Odds")    * ( 1 + ((feed$odds >= 0) * feed$odds) + ((feed$odds < 0) * 1/abs(feed$odds)) )

  feed <- feed[!is.na(feed$DecimalOdds), ]
  # Center of mass for odds
  odds.com <- sum(feed$DecimalOdds * feed$Totalbetamount) / sum(feed$Totalbetamount)
  
  # Soccer and Basketball makes up 95% of turnover, ignore the rest
  feed <- feed[(feed$GameName=="Soccer" | feed$GameName=="Basketball") & feed$BetType != "Parlay" & feed$DecimalOdds < 10,]
  sum(feed$Totalbetamount) / sum(feed.raw$Totalbetamount)
  
  # There are too many unique odds, we need to group them
  feed$DecimalOddsRounded <- round(feed$DecimalOdds, digits=1)

  # FYI print
  unique(feed$DecimalOddsRounded)
}

#
# Choose a column to cluster, either turnover, betcount, etc.
#
get.points.for.clustering <- function(npoints.desired, column.to.cluster, column.to.use.as.weight, df.feed)
{
  x <- c()
  for( margin in unique(df.feed[,column.to.cluster]) )
  {
    df.tmp <- df.feed[df.feed[,column.to.cluster] == margin, ]
    num.occurrence <- 1 + floor(sum(df.tmp[,column.to.use.as.weight]) / npoints.desired)
    x <- c( x, rep(margin, times =  num.occurrence) )
  }
  return(x)
}

column.to.cluster <- "Margin"
if(prdgrp == "SB") column.to.cluster <- "DecimalOddsRounded"
column.to.use.as.weight <- "Totalbetamount"

# Find normalizer if we only want say 50,000 points
npoints <- sum(feed[,column.to.use.as.weight]) / 50000

x <- get.points.for.clustering(npoints.desired = npoints,
                               column.to.cluster = column.to.cluster,
                               column.to.use.as.weight = column.to.use.as.weight,
                               df.feed = feed)

# Cluster the desired x
ncenters <- 5
cl <- kmeans(x=x, centers = ncenters)
df.cl <- data.frame(ClusterCenter=cl$centers, ClusterSize=cl$size, SizeProportion=cl$size/sum(cl$size))
df.cl <- df.cl[order(df.cl$ClusterCenter, decreasing = FALSE),]
# Get bottom/high limit
len <- dim(df.cl)[1]
df.cl$ClusterLeft <- c(min(x), df.cl$ClusterCenter[1:(len-1)])
df.cl$ClusterLeft <- ( df.cl$ClusterCenter + df.cl$ClusterLeft ) / 2
df.cl$ClusterLeft[1] <- min(x)
df.cl$ClusterRight <- c(df.cl$ClusterCenter[2:(len)], max(x))
df.cl$ClusterRight <- ( df.cl$ClusterCenter + df.cl$ClusterRight ) / 2
df.cl$ClusterRight[len] <- max(x)
# Get cluster size within lower and upper limit
df.cl$ClusterSizeLeft <- rep(0, len)
df.cl$ClusterSizeRight <- rep(0, len)
for(i in c(1:len))
{
  df.cl$ClusterSizeLeft[i] <- length(x[x>=df.cl$ClusterLeft[i] & x<df.cl$ClusterCenter[i]])
  df.cl$ClusterSizeRight[i] <- length(x[x>=df.cl$ClusterCenter[i] & x<df.cl$ClusterRight[i]])
}
df.cl$CheckSum = df.cl$ClusterSizeLeft + df.cl$ClusterSizeRight
# Sum of cluster size must be equal to bottom and top points within lower limit to cluster center and between cluster center to high limit
if( sum( (df.cl$ClusterSize - (df.cl$ClusterSizeLeft + df.cl$ClusterSizeRight))**2) > 0.000000000001 )
  stop("Inconsistent left/right cluster sizes")

filename <- paste("cluster.", prdgrp, ".", column.to.cluster, ".weight.", column.to.use.as.weight, ".csv", sep="")
write.csv(df.cl, filename)

df.cl

