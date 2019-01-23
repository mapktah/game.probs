
rm(list=ls())
# Avoid scientific notation such as 1e+05, so that when in Excel it will be exported correctly to DB
options(scipen=999)
setwd("~/dev/quant/gameprob")
source("lib.card.R")

# Read Crystal file
df.raw <- read.csv("livedealergames.v2.csv", header=TRUE)

# Data Cleanup
cols.str <- c("Brand", "Product", "Vendor.API.Game.Name", "Vendor.Game.Name",
              "Game.Type", "Sub.Game.Type", "Vendor.API.Product.Name",
              "Vendor.Bet.Type", "BetType", "Sub.Bet.Type")
cols.num <- c("Min.Limit", "Max.Limit", "Payout..Decimal.Odd.", "Min.Bet", "Max.Bet",
              "Decks.of.Card", "Margin", "Time..Sec.")

df <- df.raw
for(col in cols.str) {
  print(paste("Converting column [", col, "] to character factor."))
  df[,col] <- as.factor( toupper(as.character(df[,col])) )
}
for(col in cols.num) {
  print(paste("Converting column [", col, "] to numeric."))
  df[,col] <- as.numeric( df[,col] )
}

#
# Break out Brand, Vendor, Game Specific info into separate tables.
# Game info is fundamental, followed by Vendor, and lastly brand.
#
# Game info
cols.game <- c("Game.Type", "Sub.Game.Type",
               "BetType", "Sub.Bet.Type",
               "Payout..Decimal.Odd.", "Decks.of.Card",
               "Margin")
# Vendor info
cols.vendor <- c("Product", "Vendor.API.Product.Name",
                 "Vendor.API.Game.Name", "Vendor.Game.Name",
                 "Game.Type", "Sub.Game.Type",
                 "Vendor.Bet.Type",
                 "BetType", "Sub.Bet.Type",
                 "Time..Sec.")

# Brand info
cols.brand <- c("Brand", "Product",
                "Vendor.API.Game.Name", "Vendor.Game.Name",
                "Vendor.Bet.Type",
                "Min.Limit", "Max.Limit")

df.game <- unique(df[,cols.game])
df.game <- df.game[order(df.game$Game.Type, df.game$Sub.Game.Type,
                         df.game$BetType, df.game$Sub.Bet.Type,
                         df.game$Payout..Decimal.Odd., df.game$Decks.of.Card),]
write.csv(df.game, "ld.game.csv")

df.vendor <- unique(df[,cols.vendor])
df.vendor <- df.vendor[order(df.vendor$Product, df.vendor$Vendor.Game.Name, df.vendor$Vendor.Bet.Type),]
write.csv(df.vendor, "ld.vendor.csv")

df.brand <- unique(df[,cols.brand])
df.brand <- df.brand[order(df.brand$Brand, df.brand$Product, df.brand$Vendor.Game.Name, df.brand$Vendor.Bet.Type),]
write.csv(df.brand, "ld.brand.csv")

# Unique game types
game.types <- unique(df.game$Game.Type)

list.game <- list()
# Loop through all games
for(gt in game.types)
{
  list.sub.game <- list()
  
  # Sub Game Types (e.g. Under baccarat, "VIP Baccarat", "No Commission/Super 6 Baccarat", etc.)
  sub.game.types <- unique(df$Sub.Game.Type[df$Game.Type==gt])
  # Loop through all sub games
  for(sg in sub.game.types)
  {
    list.bet.types <- list()
    
    bettypes <- unique(df$BetType[df$Game.Type==gt & df$Sub.Game.Type==sg])
    
    for(bt in bettypes)
    {
      # Now retrieve the bet types
      subbettypes <- as.character( unique(df$Sub.Bet.Type[df$Game.Type==gt & df$Sub.Game.Type==sg & df$BetType==bt]) )
      # if(length(subbettypes > 0)) print(paste("Found sub bet types for game=",gt,", sub game=",sg,", bet type=",bt,", sub bet type=",subbettypes,sep=""))
      list.bet.types[[bt]] <- subbettypes 
    }
    list.sub.game[[sg]] <- list.bet.types
  }
  list.game[[gt]] <- list.sub.game
}

#
# Form a text "tree"
#
strOutput <- ""
for(gametype in names(list.game))
{
  # All brands & products with this gametype
  brands <- unique(df$Brand[df$Game.Type==gametype])
  prods  <- unique(df$Product[df$Game.Type==gametype])
  
  print( paste(gametype,
               " (Brands=", paste(brands,collapse="/"), ", Products=", paste(prods, collapse="/"), ")" , sep="") )
  
  list.sub.game.types <- list.game[[gametype]]
  
  for(subgametype in names(list.sub.game.types))
  {
    # All brands & products with this gametype
    brands <- unique(df$Brand[df$Game.Type==gametype & df$Sub.Game.Type==subgametype])
    prods  <- unique(df$Product[df$Game.Type==gametype & df$Sub.Game.Type==subgametype])
    
    print( paste("     ", subgametype,
                 " (Brands=", paste(brands, collapse="/"), ", Products=", paste(prods, collapse="/"), ")" , sep="") )
    
    list.bettypes <- list.sub.game.types[[subgametype]]
    
    for(bettype in names(list.bettypes))
    {
      # All brands & products with this gametype
      brands <- unique(df$Brand[df$Game.Type==gametype & df$Sub.Game.Type==subgametype
                                & df$BetType==bettype])
      prods  <- unique(df$Product[df$Game.Type==gametype & df$Sub.Game.Type==subgametype
                                  & df$BetType==bettype])
      margins <- unique(df$Margin[df$Game.Type==gametype & df$Sub.Game.Type==subgametype
                                  & df$BetType==bettype])
      
      print( paste("          ", bettype,
                   " (Brands=", paste(brands, collapse="/"),
                   ", Products=", paste(prods, collapse="/"),
                   ", Margins=", paste(margins, collapse="/"),
                   ")" , sep="") )
      
      list.subbettypes <- list.bettypes[[bettype]]
      
      for(subbettype in names(list.subbettypes))
      {
        # All brands & products with this gametype
        brands <- unique(df$Brand[df$Game.Type==gametype & df$Sub.Game.Type==subgametype
                                  & df$BetType==bettype & df$Sub.Bet.Type==subbettype])
        prods  <- unique(df$Product[df$Game.Type==gametype & df$Sub.Game.Type==subgametype
                                    & df$BetType==bettype & df$Sub.Bet.Type==subbettype])
        margins <- unique(df$Margin[df$Game.Type==gametype & df$Sub.Game.Type==subgametype
                                    & df$BetType==bettype & df$Sub.Bet.Type==subbettype])
        
        print( paste("          ", bettype,
                     " (Brands=", paste(brands, collapse="/"),
                     ", Products=", paste(prods, collapse="/"),
                     ", Margins=", paste(margins, collapse="/"),
                     ")" , sep="") )
      }
    }
  }
}
