# Player without any strategy

randomrat.bet <- function(bankroll){
  amount <- sample(1:bankroll, size=1)
  bankroll <- bankroll - amount
  return(amount)
}

randomrat.turn <- function(bank.hand, my.hand){
  response = sample(c("pass", "hit", "double", "split"), size=1)
  return(response)
}

randomrat.play <- function() {
  repeat {
    card <- dealCard(deck)

    if (card$Face == "Ace") {
      if(sum(randomrat.hand$Value) <= 10) {
        card$Value[card$Face == "Ace"] <- 11
      } else {
        card$Value[card$Face == "Ace"] <- 1
      }
    } else {
      print("")
    }

    turn = randomrat.turn()

    if(turn == "hit") {
      randomrat.hand <<- rbind(randomrat.hand, dealCard(deck))
    } else if(turn == "pass") {
      break;
    } else if(turn == "double") {
      ## originele bet moet je dan hebben ff overnadenken nog
    } else {
      ## originele bet moet je dan hebben voor de split
    }
  }
  result <<- rbind(result, c("CopyCat", sum(copycat.hand$Value)))
}

