# Game loop for multiple Blackjack player and the bank

source("RandomRat.R")
source("CopyCat.R")
source("Dealer.R")


# test randomRat
play.randomRat <- function() {
  RandomRat.bankroll <<- 50000
  
  randomrat.hand <<- createHand() 
  
  randomrat.play()

  print(paste("RandomRat bankroll: ", randomrat.bet(RandomRat.bankroll)))
  print(paste("Sum randomRat handvalue: ", sum(randomrat.hand$Value)))
}

# test Copycat
play.copycat <- function() {
  Copycat.bankroll <- 50000
  Copycat.bet <- 5000
  
  copycat.hand <<- createHand() 
  
  copycat.turn(copycat.hand)
  
  print(paste("Copycat bets: ", copycat.bet(Copycat.bankroll, Copycat.bet)))
  print(paste("Sum CopyCat handvalue: ", sum(copycat.hand$Value)))
}

# bank
handleBank <- function() {
  player.hand <<- createHand() 

  repeat {
    card <- dealCard(deck)

    if (card$Face == "Ace") {
      if(sum(player.hand$Value) <= 10) {
        card$Value[card$Face == "Ace"] <- 11
      } else {
        card$Value[card$Face == "Ace"] <- 1
      }
    } else {
      player.hand <<- rbind(player.hand, dealCard(deck))
    }

    if (sum(player.hand$Value) > 17) {
      break;
    }
  }

  resultBank <<- data.frame("name" = "Bank", "Value" = sum(player.hand$Value))
}

playGame <- function(players) {
  # shuffle deck with the new created deck
  deck <- shuffleDeck(createDeck())

  # handle player 1
  result <<- data.frame(players = c(1), score = c(1))
  colnames(result) <- c("PlayerName", "Value")

  for(player in players) {
    if(player == "CopyCat") {
      play.copycat()
    } else if(player == "RandomRat") {
      play.randomRat()
    } else {
      print("Ik heb geen strategie ik doe niet mee")
    }
  }

  #handleBank()

  #winners <- selectWinner()
  #print(winners)
}

playGame(players= c("CopyCat", "RandomRat"))
