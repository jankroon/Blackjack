# Game loop for multiple Blackjack player and the bank
# Not 100% error free :-(

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
  
  # ADD result to resultlist
  result = rbind(result,list("RandomRat",sum(randomrat.hand$Value)))
  
}

# test Copycat
play.copycat <- function() {
  Copycat.bankroll <- 50000
  Copycat.bet <- 5000
  
  copycat.hand <<- createHand() 
  
  copycat.turn(copycat.hand)
  
  print(paste("Copycat bets: ", copycat.bet(Copycat.bankroll, Copycat.bet)))
  print(paste("Sum CopyCat handvalue: ", sum(copycat.hand$Value)))
  
  # ADD result to resultlist
  result = rbind(result,list("CopyCat",sum(copycat.hand$Value)))
}

selectWinner <- function() {
  
  if(resultBank$Value == 21) {
    return("Bank won")
  } else if(resultBank$Value > 21) {
    return(result[(which(result$score <= 21)),])
  } else {
    winner1 <- result[(which(result$score == 21)),]
    winner2 <- result[(which(result$score > resultBank$Value) & (result$score < 21)),]
    result[(which(result$score == 21 || (result$score > resultBank$Value) & (result$score < 21))),]
    
    # return niets --> return if playerscore is 21 en if playerscore groter is dan bank maar kleiner is of gelijk aan 21
    # result[(which(result$score == 21 || (result$score > resultBank$Value) & (result$score < 21))),]
    winner <- rbind(winner2, winner1)
    return(winner)
  }
}

# bank
handleBank <- function() {
  bank.hand <<- createHand() 

  repeat {
    card <- dealCard(deck)

    if (card$Face == "Ace") {
      if(sum(bank.hand$Value) <= 10) {
        card$Value[card$Face == "Ace"] <- 11
      } else {
        card$Value[card$Face == "Ace"] <- 1
      }
    } else {
      bank.hand <<- rbind(bank.hand, dealCard(deck))
    }

    if (sum(bank.hand$Value) > 17) {
      break;
    }
  }

  resultBank <<- data.frame("name" = "Bank", "Value" = sum(bank.hand$Value))
}

playGame <- function(players) {
  # shuffle deck with the new created deck
  # ISSUE: deck is not shuffled !!!
  deck <- createDeck()
  deck <- shuffleDeck(deck)

  # handle player 1
  result <<- data.frame(players, score = c(0))
  colnames(result) <- c("PlayerName", "Value")
  # ISSUE: first row is 1  1 changed in playername 0
  
  

  for(player in players) {
    if(player == "CopyCat") {
      play.copycat()
    } else if(player == "RandomRat") {
      play.randomRat()
    } else {
      print("Ik heb geen strategie ik doe niet mee")
    }
  }

  handleBank()
  
  print("Bank:")
  print(resultBank)

  winners <- selectWinner()
  print(winners)
}

playGame(players= c("CopyCat","RandomRat"))
