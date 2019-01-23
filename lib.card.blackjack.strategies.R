#######################################################################################
#
# There is no meaning to talk about probabilities in BJ if there is no strategy.
# See strategies here: https://wizardofodds.com/games/blackjack/strategy/calculator/
# Below is configured for:
#   DK8 (8 decks) EU (European no peek), S17 (stand on soft 17), DAS (double after split), NOSU (no surrender).
# Row "4" is needed for (2,2) split and drawing a 2 again.
BJ.STRATEGY.BASIC.PLAYERHARD.DK8.EU.S17.DAS.NOSU <- t ( data.frame(
  "4" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "5" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "6" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "7" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "8" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "9" = c( "H","Dh","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "10"= c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "11"= c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "12"= c( "H", "H", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "13"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "14"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "15"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "16"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "17"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "18"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "19"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "20"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "21"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.PLAYERHARD.DK8.EU.S17.DAS.NOSU) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Hard Score
rownames(BJ.STRATEGY.BASIC.PLAYERHARD.DK8.EU.S17.DAS.NOSU) <- c("4", "5", "6", "7", "8", "9", "10", "11",
                                                            "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

# The row for 12 soft is added for the case A-A split and draw A again with no more split
BJ.STRATEGY.BASIC.PLAYERSOFT.DK8.EU.S17.DAS.NOSU <- t ( data.frame(
  "12"= c( "H", "H", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "13"= c( "H", "H", "H","Dh","Dh", "H", "H", "H", "H", "H" ),
  "14"= c( "H", "H", "H","Dh","Dh", "H", "H", "H", "H", "H" ),
  "15"= c( "H", "H","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "16"= c( "H", "H","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "17"= c( "H","Dh","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "18"= c( "S","Ds","Ds","Ds","Ds", "S", "S", "H", "H", "H" ),
  "19"= c( "S", "S", "S", "S","Ds", "S", "S", "S", "S", "S" ),
  "20"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "21"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.PLAYERSOFT.DK8.EU.S17.DAS.NOSU) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Soft Score
rownames(BJ.STRATEGY.BASIC.PLAYERSOFT.DK8.EU.S17.DAS.NOSU) <- c("12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

BJ.STRATEGY.BASIC.PLAYERPAIR.DK8.EU.S17.DAS.NOSU <- t ( data.frame(
  "4"  = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "6"  = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "8"  = c( "H", "H", "H", "P", "P", "H", "H", "H", "H", "H" ),
  "10" = c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "12" = c( "P", "P", "P", "P", "P", "H", "H", "H", "H", "H" ),
  "14" = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "16" = c( "P", "P", "P", "P", "P", "P", "P", "P", "H", "H" ),
  "18" = c( "P", "P", "P", "P", "P", "S", "P", "P", "S", "S" ),
  "20" = c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "2"  = c( "P", "P", "P", "P", "P", "P", "P", "P", "P", "H" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.PLAYERPAIR.DK8.EU.S17.DAS.NOSU) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Pair
rownames(BJ.STRATEGY.BASIC.PLAYERPAIR.DK8.EU.S17.DAS.NOSU) <- c("4", "6", "8", "10", "12", "14", "16", "18", "20", "2")

BJ.STRATEGY.BASIC.DK8.EU.S17.DAS.NOSU <- list("PlayerHard"=BJ.STRATEGY.BASIC.PLAYERHARD.DK8.EU.S17.DAS.NOSU,
                                          "PlayerSoft"=BJ.STRATEGY.BASIC.PLAYERSOFT.DK8.EU.S17.DAS.NOSU,
                                          "PlayerPair"=BJ.STRATEGY.BASIC.PLAYERPAIR.DK8.EU.S17.DAS.NOSU)

#######################################################################################
# Below is configured for:
#   DK8 (4 decks) EU (European no peek), S17 (stand on soft 17), DAS (double after split), SU (Surrender).
# Row "4" is needed for (2,2) split and drawing a 2 again.
BJ.STRATEGY.BASIC.PLAYERHARD.DK4.EU.S17.DAS.SU <- t ( data.frame(
  "4" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H","Sh" ),
  "5" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H","Sh" ),
  "6" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H","Sh" ),
  "7" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "8" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "9" = c( "H","Dh","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "10"= c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "11"= c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "12"= c( "H", "H", "S", "S", "S", "H", "H", "H", "H","Sh" ),
  "13"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H","Sh" ),
  "14"= c( "S", "S", "S", "S", "S", "H", "H", "H","Sh","Sh" ),
  "15"= c( "S", "S", "S", "S", "S", "H", "H", "H","Sh","Sh" ),
  "16"= c( "S", "S", "S", "S", "S", "H", "H","Sh","Sh","Sh" ),
  "17"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S","Ss" ),
  "18"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "19"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "20"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "21"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.PLAYERHARD.DK4.EU.S17.DAS.SU) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Hard Score
rownames(BJ.STRATEGY.BASIC.PLAYERHARD.DK4.EU.S17.DAS.SU) <- c("4", "5", "6", "7", "8", "9", "10", "11",
                                                            "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

# The row for 12 soft is added for the case A-A split and draw A again with no more split
BJ.STRATEGY.BASIC.PLAYERSOFT.DK4.EU.S17.DAS.SU <- t ( data.frame(
  "12"= c( "H", "H", "S", "S", "S", "H", "H", "H", "H","Sh" ),
  "13"= c( "H", "H", "H","Dh","Dh", "H", "H", "H", "H", "H" ),
  "14"= c( "H", "H", "H","Dh","Dh", "H", "H", "H", "H", "H" ),
  "15"= c( "H", "H","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "16"= c( "H", "H","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "17"= c( "H","Dh","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "18"= c( "S","Ds","Ds","Ds","Ds", "S", "S", "H", "H", "H" ),
  "19"= c( "S", "S", "S", "S","Ds", "S", "S", "S", "S", "S" ),
  "20"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "21"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.PLAYERSOFT.DK4.EU.S17.DAS.SU) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Soft Score
rownames(BJ.STRATEGY.BASIC.PLAYERSOFT.DK4.EU.S17.DAS.SU) <- c("12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

BJ.STRATEGY.BASIC.PLAYERPAIR.DK4.EU.S17.DAS.SU <- t ( data.frame(
  "4"  = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "6"  = c( "P", "P", "P", "P", "P", "P", "H", "H", "H","Sh" ),
  "8"  = c( "H", "H", "H", "P", "P", "H", "H", "H", "H", "H" ),
  "10" = c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "12" = c( "P", "P", "P", "P", "P", "H", "H", "H", "H","Sh" ),
  "14" = c( "P", "P", "P", "P", "P", "P", "H", "H","Sh","Sh" ),
  "16" = c( "P", "P", "P", "P", "P", "P", "P", "P","Sh","Sh" ),
  "18" = c( "P", "P", "P", "P", "P", "S", "P", "P", "S", "S" ),
  "20" = c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "2"  = c( "P", "P", "P", "P", "P", "P", "P", "P", "P", "H" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.PLAYERPAIR.DK4.EU.S17.DAS.SU) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Pair
rownames(BJ.STRATEGY.BASIC.PLAYERPAIR.DK4.EU.S17.DAS.SU) <- c("4", "6", "8", "10", "12", "14", "16", "18", "20", "2")

BJ.STRATEGY.BASIC.DK4.EU.S17.DAS.SU <- list("PlayerHard"=BJ.STRATEGY.BASIC.PLAYERHARD.DK4.EU.S17.DAS.SU,
                                            "PlayerSoft"=BJ.STRATEGY.BASIC.PLAYERSOFT.DK4.EU.S17.DAS.SU,
                                            "PlayerPair"=BJ.STRATEGY.BASIC.PLAYERPAIR.DK4.EU.S17.DAS.SU)

#######################################################################################
#
# From https://en.wikipedia.org/wiki/Blackjack
# Below is configured for:
#   DK4-8 (4-8 decks), EU/AM, S17 (stand on soft 17), DAS (double after split), NOSU (no surrender), OBL (only original bet lost for dealer BJ).
# The row for 4 soft is added for the case 2-2 split and draw 2 again with no more split
BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERHARD <- t ( data.frame(
  "4" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "5" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "6" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "7" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "8" = c( "H", "H", "H", "H", "H", "H", "H", "H", "H", "H" ),
  "9" = c( "H","Dh","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "10"= c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "11"= c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H" ),
  "12"= c( "H", "H", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "13"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "14"= c( "S", "S", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "15"= c( "S", "S", "S", "S", "S", "H", "H", "H","Sh", "H" ),
  "16"= c( "S", "S", "S", "S", "S", "H", "H","Sh","Sh","Sh" ),
  "17"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "18"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "19"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "20"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "21"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERHARD) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Hard Score
rownames(BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERHARD) <- c("4", "5", "6", "7", "8", "9", "10", "11",
                                                      "12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

# The row for 12 soft is added for the case A-A split and draw A again with no more split
BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERSOFT <- t ( data.frame(
  "12"= c( "H", "H", "S", "S", "S", "H", "H", "H", "H", "H" ),
  "13"= c( "H", "H", "H","Dh","Dh", "H", "H", "H", "H", "H" ),
  "14"= c( "H", "H", "H","Dh","Dh", "H", "H", "H", "H", "H" ),
  "15"= c( "H", "H","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "16"= c( "H", "H","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "17"= c( "H","Dh","Dh","Dh","Dh", "H", "H", "H", "H", "H" ),
  "18"= c( "S","Ds","Ds","Ds","Ds", "S", "S", "H", "H", "H" ),
  "19"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "20"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "21"= c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERSOFT) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Soft Score
rownames(BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERSOFT) <- c("12", "13", "14", "15", "16", "17", "18", "19", "20", "21")

BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERPAIR <- t ( data.frame(
  "4"  = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "6"  = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "8"  = c( "H", "H", "H", "P", "P", "H", "H", "H", "H", "H" ),
  "10" = c("Dh","Dh","Dh","Dh","Dh","Dh","Dh","Dh", "H", "H" ),
  "12" = c( "P", "P", "P", "P", "P", "H", "H", "H", "H", "H" ),
  "14" = c( "P", "P", "P", "P", "P", "P", "H", "H", "H", "H" ),
  "16" = c( "P", "P", "P", "P", "P", "P", "P", "P", "P", "P" ),
  "18" = c( "P", "P", "P", "P", "P", "S", "P", "P", "S", "S" ),
  "20" = c( "S", "S", "S", "S", "S", "S", "S", "S", "S", "S" ),
  "2"  = c( "P", "P", "P", "P", "P", "P", "P", "P", "P", "P" )
) )
# Banker Face Card
colnames(BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERPAIR) <- c("2", "3", "4", "5", "6", "7", "8", "9", "10", "1")
# Player Pair
rownames(BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERPAIR) <- c("4", "6", "8", "10", "12", "14", "16", "18", "20", "2")

BJ.STRATEGY.BASIC.WIKIPEDIA <- list("PlayerHard"=BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERHARD,
                                    "PlayerSoft"=BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERSOFT,
                                    "PlayerPair"=BJ.STRATEGY.BASIC.WIKIPEDIA.PLAYERPAIR)

#######################################################################################

# The numbers will get large, so we use the smallest. Using 100 is convenient but it will
# fail due to too many cards. e.g. 2010101010302020352 will make R fail.
CARDS.CODING.BASE <- 11

bj.get.rules.structure <- function(
  # These 3 rules will affect our player card generation
  max.split,
  double.after.split,
  split.ace.draw.one.more.card.only,
  # These rules will only affect calculation of probability and results
  no.decks,
  split.no.blackjack,
  split.lose.only.original.stake.if.dealer.bj,
  can.surrender,
  # These rules only affect dealer, and results
  dealer.hit.on.soft.17)
{
  return( data.frame(no.decks = no.decks,
                     # How many times a player can split, usually once or twice
                     max.split = max.split,
                     # Can a player double after split, or DDAS (double down after split)
                     double.after.split = double.after.split,
                     # When splitting ace, draw only one more card to both split hands only
                     split.ace.draw.one.more.card.only = split.ace.draw.one.more.card.only,
                     # After splitting, BJ don't count anymore
                     split.no.blackjack = split.no.blackjack,
                     split.lose.only.original.stake.if.dealer.bj = split.lose.only.original.stake.if.dealer.bj,
                     can.surrender  = can.surrender,
                     dealer.hit.on.soft.17 = dealer.hit.on.soft.17) )
}

bj.get.rules.structure.standard <- function()
{
  return(  bj.get.rules.structure(no.decks                 = 8,
                                  max.split                = 1,
                                  double.after.split                = TRUE,
                                  split.ace.draw.one.more.card.only = TRUE,
                                  split.no.blackjack                = TRUE,
                                  split.lose.only.original.stake.if.dealer.bj = FALSE,
                                  can.surrender = FALSE,
                                  dealer.hit.on.soft.17 = FALSE) )
}

bj.get.cards.encoding <- function(vcards)
{
  pow <- 0
  val <- 0
  for(card in vcards)
  {
    val <- val + ( card * (CARDS.CODING.BASE**pow) )
    pow <- pow + 1
  }
  return(val)
}

# We need to break encoding into cards 1-10, then 11 upwards, otherwise encoding numbers get too big causing error.
bj.get.individualcards.from.cardsencoding <- function(cardsencoding, cardsencoding11=0)
{
  if(is.na(cardsencoding)) return(c())
  # Get cards
  vcards <- c()
  n <- 0
  while(cardsencoding > 0)
  {
    cardvalue     <- (cardsencoding %% CARDS.CODING.BASE)
    vcards        <- c(vcards, cardvalue)
    cardsencoding <- floor(cardsencoding/CARDS.CODING.BASE)
  }
  while(cardsencoding11 > 0)
  {
    cardvalue       <- (cardsencoding11 %% CARDS.CODING.BASE)
    vcards          <- c(vcards, cardvalue)
    cardsencoding11 <- floor(cardsencoding11/CARDS.CODING.BASE)
  }
  return(vcards)
}

bj.get.hardscore.from.cardsencoding <- function(cardsencoding, cardsencoding11=0)
{
  return(sum(bj.get.individualcards.from.cardsencoding(cardsencoding=cardsencoding, cardsencoding11=cardsencoding11)))
}

bj.get.softscore.from.cardsencoding <- function(cardsencoding, cardsencoding11=0)
{
  vcards <- bj.get.individualcards.from.cardsencoding(cardsencoding=cardsencoding, cardsencoding11=cardsencoding11)
  hscore <- sum(vcards)
  if(is.element(1,vcards))
  {
    if(sum(vcards)+10<=21) return(sum(vcards)+10)
  }
  return(hscore)
}

bj.sanitycheck.playercards <- function(df)
{
  # Sanity checks
  vcards <- bj.get.individualcards.from.cardsencoding(cardsencoding=df$cardsencoding, cardsencoding11=df$cardsencoding11)
  score.hard <- bj.get.hardscore.from.cardsencoding(cardsencoding=df$cardsencoding, cardsencoding11=df$cardsencoding11)
  score.soft <- bj.get.softscore.from.cardsencoding(cardsencoding=df$cardsencoding, cardsencoding11=df$cardsencoding11)

  if(sum(vcards) != score.hard)
    stop(print(paste("Hard Score of cards not same with derived, cards [", df$cardstring, "] but hard score [", df$score.hard, "]",sep="")))
  if(score.hard != df$score.hard)
    stop(print(paste("Hard Score error, cards [", df$cardstring, "] but hard score [", df$score.hard, "], derived hard score [", score.hard, "]", sep="")))
  if(score.soft != df$score)
    stop(print(paste("Soft Score error, cards [", df$cardstring, "] hard [", df$score.hard, "] but soft score [", df$score, "], derived soft score [", score.soft, "]", sep="")))
  if(df$hardsoft == "hard" & df$score!=df$score.hard)
    stop(print(paste("Hard but soft score different, cards [", df$cardstring, "] soft score [", df$score, "], hard score [", df$score.hard, "]", sep="")))
  if(df$hardsoft == "soft" & df$score!=df$score.hard+10)
    stop(print(paste("Soft but soft score not hard score+10, cards [", df$cardstring, "] soft score [", df$score, "], hard score [", df$score.hard, "]", sep="")))
  
  return(TRUE)
}

#
# If player splits or doubles, we create extra rows.
# This way a single data frame holds all the player hands.
# Dealer/Player Cards are coded as base 100 number, with 1-10 coded as 1-10.
# So if the cards are [1,7,5,6], it will be coded as (0)6050701
#
bj.get.player.attributes <- function(facecard=0, holecard=0, cardencoding=0, cardsencoding11=0, cardstring="",
                                     score.hard=0, score=0,
                                     totalcards=0, hardsoft="hard", playertype="player",
                                     double.no.more.hit=FALSE, splitlevel=0, surrender=FALSE, nextdecision=NA)
{
  df <- data.frame(playertype=playertype, facecard=facecard, holecard=holecard,
                   cardsencoding=cardencoding, cardsencoding11=cardsencoding11, cardstring=cardstring,
                   score.hard=score.hard, score=score, totalcards=totalcards,
                   hardsoft=hardsoft, doublenomorehit=double.no.more.hit, surrender=surrender,
                   splitlevel=splitlevel, nextdecision=nextdecision)
  df$cardstring   <- as.character(df$cardstring)
  df$hardsoft     <- as.character(df$hardsoft)
  df$nextdecision <- as.character(df$nextdecision)
  
  bj.sanitycheck.playercards(df)
  
  return(df)
}

bj.add.card.to.player <- function(rules, newcard, df)
{
  df$totalcards    <- df$totalcards + 1
  # Need to split cards encoding into first 10 cards, and 11th onwards, otherwise encoding number gets too big
  if(df$totalcards<=10) {
    df$cardsencoding <- df$cardsencoding + ( newcard * (CARDS.CODING.BASE**(df$totalcards-1)) )
  } else {
    df$cardsencoding11 <- df$cardsencoding11 + ( newcard * (CARDS.CODING.BASE**(df$totalcards-10-1)) )
  }
  df$cardstring    <- paste(bj.get.individualcards.from.cardsencoding(cardsencoding=df$cardsencoding, cardsencoding11=df$cardsencoding11), collapse=",")
  df$score.hard    <- df$score.hard + newcard
  df$score         <- df$score + newcard + ( 10 * ((newcard==1) & (df$score+newcard<12)) )
  # If soft score busts, bring it back to hard score
  if(df$score > 21) df$score <- df$score.hard
  
  if(df$totalcards==1) {
    df$facecard <- newcard
  } else if(df$totalcards==2) {
    # Second card only record once (not at splits)
    if(df$splitlevel == 0)
      df$holecard <- newcard
    # Otherwise, card already split and second card drawn.
    # In this case we update the holecard with the facecard, not the new card
    else
      df$holecard <- df$facecard
    
    # Use newcard to check, because holecard is always the first 2nd card drawn
    if(df$splitlevel < rules$max.split) {
      if(newcard == df$facecard & df$playertype=="player") df$hardsoft <- "pair"
    }
  }
  
  # We don't update the status for pair
  if(df$hardsoft != "pair")
  {
    if(df$score == df$score.hard) {
      df$hardsoft = "hard"
    } else if(df$score - df$score.hard == 10) {
      df$hardsoft = "soft"
    } else {
      stop(paste("Inconsistency in card scores. Hard score [", df$score.hard, "] , soft score [", df$score,
                 "] card string [", df$cardstring, "]"))
    }
  }
  
  bj.sanitycheck.playercards(df)
  
  return(df)
}

#
# Key in making decision to hit, stand, double, or split
#
bj.strategy.get.decision <- function(rules, playerstrategy, playerdf, dealerfacecard, verbose=FALSE)
{
  rows <- dim(playerdf)[1]
  for(i in c(1:rows))
  {
    # If player already bust, or split with Ace and draw only one more card for Ace split is TRUE
    if( (playerdf$score[i] > 21) |
        (playerdf$splitlevel[i]>0 & playerdf$facecard[i]==1 & playerdf$totalcards[i]==2 & rules$split.ace.draw.one.more.card.only) )
    {
      playerdf$nextdecision[i] <- "S"
      next
    }
    
    decision <- "H"
    if( (playerdf$totalcards[i]==1) ) {
      decision <- "H"
    }
    else {
      rowidentifier <- as.character(playerdf$score[i])
      bjstrategy <- playerstrategy[["PlayerHard"]]
      
      if(playerdf$hardsoft[i] == "soft")
      {
        bjstrategy <- playerstrategy[["PlayerSoft"]]
      }
      else if(playerdf$hardsoft[i] == "pair")
      {
        bjstrategy <- playerstrategy[["PlayerPair"]]
        rowidentifier <- as.character(playerdf$score.hard[i])
      }
      
      # Get Strategy
      if( !is.element(rowidentifier, rownames(bjstrategy)) )
      {
        stop(paste("Error: pscore.hard=", playerdf$score.hard[i], ", pscore=", playerdf$score[i],
                   ", playerhardsoft=", playerdf$hardsoft[i], ", rowidentifier=", rowidentifier,
                   ", playertotalcards=", playerdf$totalcards[i],
                   ", dealerfacecard=", dealerfacecard, " don't exist bjstrategy", sep=""))
      }
      
      decision <- bjstrategy[rowidentifier, as.character(dealerfacecard)]
    }
    
    playerdf$nextdecision[i] <- decision
    
    if(verbose) print(paste("Player [", i ,"], decision: [", decision, "], pscore ", playerdf$score[i], " (hard=", playerdf$score.hard[i],
                            ") playerhardsoft [", playerdf$hardsoft[i], "], playertotalcards ", playerdf$totalcards[i],
                            ", Dealer Face Card ", dealerfacecard, sep=""))
    
    # Due to previous doubling or surrender
    if(playerdf$doublenomorehit[i] | playerdf$surrender[i])
    {
      playerdf$nextdecision[i] <- "S"
    }
    else
    {
      # We need to change the mode from pair to either hard/soft
      if(playerdf$hardsoft[i] == "pair") {
        playerdf$hardsoft[i] <- "hard"
        if(playerdf$score.hard[i] == 2)
          playerdf$hardsoft[i] <- "soft"
      }
      
      # If double and hit, make sure no more hits in next call, only for first 2 cards.
      # TODO: Handle case where after splits, player not allowed to double.
      if( (decision=="Dh") ) {
        playerdf$nextdecision[i] <- "H"
        playerdf$doublenomorehit[i]    <- ( playerdf$totalcards[i]==2 &
                                              ( playerdf$splitlevel[i]==0 | (playerdf$splitlevel[i]>0 & rules$double.after.split) ) )
      } else if( (decision=="Ds") ) {
        playerdf$nextdecision[i] <- "S"
        playerdf$doublenomorehit[i]    <- ( playerdf$totalcards[i]==2 &
                                              ( playerdf$splitlevel[i]==0 | (playerdf$splitlevel[i]>0 & rules$double.after.split) ) )
      }
      # The flag for doublenomorehit once set to TRUE is never set back to FALSE
      
      # TODO: Handle "Sh" (Surrender, or if not allowed then Hit)
      if( playerdf$nextdecision[i]=="Sh" | playerdf$nextdecision[i] == "Ss")
      {
        if(rules$can.surrender) {
          playerdf$nextdecision[i] <- "S"
          playerdf$surrender[i]    <- TRUE
        } else {
          # Can't surrender
          if(playerdf$nextdecision[i] == "Sh")
            playerdf$nextdecision[i] <- "H"
          else
            playerdf$nextdecision[i] <- "S"
        }
      }
      
      # Check if decision is to split, and still allowed to split
      if(decision == "P")
      {
        if(playerdf$splitlevel[i] < rules$max.split)
        {
          # Create a new row, and modify current row
          p1 <- playerdf$score.hard[i] / 2
          playerdf.split <- bj.get.player.attributes()
          playerdf.split <- bj.add.card.to.player(newcard=p1, df=playerdf.split)
          playerdf.split$nextdecision[i] <- "H"
          playerdf.split$splitlevel[i]   <- playerdf$splitlevel[i] + 1
          playerdf[i,] <- playerdf.split
          
          # Split out a row
          newsplitrow <- playerdf[i,]
          # Clone another row as split
          playerdf <- rbind(playerdf, newsplitrow)
        }
        else
        {
          # Throw Exception
          stop(paste("Error, player already split [", playerdf$splitlevel[i], "] times, but maxsplit is only [",
                     rules$max.split, "]. Should have been handled in bj.add.card.to.player() function.", sep=""))
        }
      }
    }
  }
  
  rows <- dim(playerdf)[1]
  for(i in c(1:rows))
  {
    bj.sanitycheck.playercards(df=playerdf[i,])
  }
  return(playerdf)
}

test.bj.strategy.get.decision <- function()
{
  rules <- bj.get.rules.structure.standard()
  rules$max.split <- 2
  
  pdf <- bj.get.player.attributes()
  pdf <- bj.add.card.to.player(rules=rules, newcard=2, df=pdf)
  pdf <- bj.add.card.to.player(rules=rules, newcard=2, df=pdf)
  pdf$splitlevel <- 0
  
  ddf <- bj.get.player.attributes(playertype = "dealer")
  ddf <- bj.add.card.to.player(rules=rules, newcard=2, df=ddf)
  
  pdf <- bj.strategy.get.decision(rules = rules, playerstrategy = BJ.STRATEGY.BASIC.WIZARDOFODDS,
                                  playerdf = pdf, dealerfacecard = ddf$facecard, verbose=TRUE)
  pdf <- pdf[1,]
  pdf <- bj.add.card.to.player(rules=rules, newcard=2, df=pdf)
  pdf <- bj.strategy.get.decision(rules = rules, playerstrategy = BJ.STRATEGY.BASIC.WIZARDOFODDS,
                                  playerdf = pdf, dealerfacecard = ddf$facecard, verbose=TRUE)
  pdf <- pdf[1,]
  pdf <- bj.add.card.to.player(rules=rules, newcard=2, df=pdf)
  pdf <- bj.strategy.get.decision(rules = rules, playerstrategy = BJ.STRATEGY.BASIC.WIZARDOFODDS,
                                  playerdf = pdf, dealerfacecard = ddf$facecard, verbose=TRUE)
}

bj.get.combination <- function(pcombs, combsmultiplier, playerdf, dealerdf, type="both")
{
  cols <- colnames(playerdf)
  
  pcols <- paste("p.", cols, sep="")
  dcols <- paste("d.", cols, sep="")
  
  colnames(playerdf) <- pcols
  colnames(dealerdf) <- dcols
  
  if(type=="both")
    return(data.frame(Combinations=pcombs, CombsMultiplier=combsmultiplier, playerdf, dealerdf))
  else if(type=="player")
    return(data.frame(playerdf))
  else if(type=="dealer")
    return(data.frame(dealerdf))
  else
    stop(paste("Type [", type, "] not supported.", sep=""))
}

BJ.RESULT.MIN.ERROR.TOLERANCE <- 0.000000000001

bj.get.result.structure <- function()
{
  # First row keeps results of original hand, 2nd row keeps hands doubled or split
  dfresult <- data.frame(ResPlayerBustDealerBust=c(0,0), ResPlayerBustDealerOk=c(0,0), ResPlayerBustDealerBJ=c(0,0),
                         ResPlayerBJDealerBust=c(0,0), ResPlayerBJDealerOk=c(0,0), ResPlayerBJDealerBJ=c(0,0),
                         ResPlayerOkDealerBust=c(0,0),
                         ResPlayerOkDealerOkWin=c(0,0), ResPlayerOkDealerOkTie=c(0,0), ResPlayerOkDealerOkLose=c(0,0),
                         ResPlayerOkDealerBJ=c(0,0))
  return(dfresult)
}

bj.get.result.columnnames <- function()
{
  cols <- colnames(bj.get.result.structure())
  return( cols )
}

#
# Blackjack results from player/dealer combinations
#
bj.get.results.of.player.dealer.combinations <- function(rules, df)
{
  # We can't include surrenders here, because it might make this 0 or negative, such that our result becomes 0,
  # which will screw up our total probability check.
  extra.from.doubles.splits <- (1*df$p.doublenomorehit + 1*df$p.splitlevel + 1)
  
  # Bust Flags
  playerbust <- (df$p.score > 21)
  dealerbust <- (df$d.score > 21)
  # BJ Flags
  playerbj <- ( df$p.score==21 & df$p.totalcards==2 & (df$p.splitlevel==0 | (df$p.splitlevel>0 & !rules$split.no.blackjack)) )
  dealerbj <- ( df$d.score==21 & df$d.totalcards==2 )
  playerok <- ( df$p.score<=21 & !playerbj )
  dealerok <- ( df$d.score<=21 & !dealerbj )
  playerscorehigher <- (df$p.score > df$d.score)
  scoretie          <- (df$p.score == df$d.score)
  playerscorelower  <- (df$p.score < df$d.score)
  
  # Player Bust First, regardless of Dealer cards
  df$ResPlayerBustDealerBust <- (1 * playerbust) * (1 * dealerbust)
  df$ResPlayerBustDealerOk   <- (1 * playerbust) * (1 * dealerok)
  df$ResPlayerBustDealerBJ   <- (1 * playerbust) * (1 * dealerbj)
  
  df$ResPlayerBJDealerBust   <- (1 * playerbj) * (1 * dealerbust)
  df$ResPlayerBJDealerOk     <- (1 * playerbj) * (1 * dealerok)
  df$ResPlayerBJDealerBJ     <- (1 * (dealerbj & playerbj))
  
  df$ResPlayerOkDealerBust   <- (1 * playerok) * (1 * dealerbust)
  df$ResPlayerOkDealerOkWin  <- (1 * (playerok & dealerok)) * (1 * playerscorehigher)
  df$ResPlayerOkDealerOkTie  <- (1 * (playerok & dealerok)) * (1 * scoretie)
  df$ResPlayerOkDealerOkLose <- (1 * (playerok & dealerok)) * (1 * playerscorelower)
  df$ResPlayerOkDealerBJ     <- (1 * playerok) * (1 * dealerbj)
  
  # Sanity check
  # We expect each line to have only 1 result
  sum.expected <- dim(df)[1]
  sum.results  <- sum(df[,bj.get.result.columnnames()])
  if(sum.expected != sum.results)
    stop(paste("Error in results: Expected sum of results = ", sum.expected, ", got ", sum.results, ".", sep=""))
  
  # Multiply from the doubles/splits
  # This only adds the count to become greater than 1. When later taking probabilities,
  # we can always check consistency by adding up values>0 to sum up to 1.
  for(col in bj.get.result.columnnames())
    df[,col] <- extra.from.doubles.splits * df[,col]
  
  # Surrenders need to be handled manually when calculating margins
  
  return(df)
}

bj.get.result <- function(rules, playerdf, dealerdf)
{
  colnames(playerdf) <- paste("p.", colnames(playerdf), sep="")
  colnames(dealerdf) <- paste("d.", colnames(dealerdf), sep="")
  df <- cbind(playerdf, dealerdf)
  
  res.tmp <- bj.get.results.of.player.dealer.combinations(rules=rules, df=df)
  res.tmp <- res.tmp[,bj.get.result.columnnames()]
  # Pure result
  df.result.tmp1 <- colSums(x=(1*(res.tmp>0)))
  # Including results of doubles & splits
  df.result.tmp2 <- colSums(x=res.tmp)
  
  df.result.tmp <- rbind(df.result.tmp1, df.result.tmp2)
  colnames(df.result.tmp) <- gsub(pattern="^Res", replacement="", colnames(df.result.tmp))
  # Add metadata column names
  #df.metadata <- data.frame(Decks=rep(rules$no.decks,2), DealerFaceCard=rep(dealerfacecard,2),
  #                          P1=c(p1,p1), P2=c(p2,p2), MaxSplit=rep(rules$max.split,2), ResultType=c("Single","All"),
  #                          TotalProb=rep(total.probability,2))
  #df.result.tmp <- cbind(df.metadata, df.result.tmp)
  
  return(df.result.tmp)
}

bj.read.all.results <- function(dirpath.strategy, resulttype)
{
  df.result <- data.frame()
  for(dealerfacecard in c(2:10,1))
  {
    fname <- paste(dirpath.strategy, "/result/res.df.", dealerfacecard, ".csv", sep="")
    df <- read.csv(fname, header=TRUE)
    # Extract only single results
    dfs <- df[df$ResultType==resulttype,]
    
    df.result <- rbind(df.result, dfs)
  }
  return(df.result)
}

