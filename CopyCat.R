# Player with strategy "do whatever the bank does"

copycat.bet <- function(bankroll, bet){
  bankroll <- bankroll - bet
  return(bet)
}

copycat.turn <- function(copycat.hand) {
  card <- dealCard(deck)

  if (card$Face == "Ace") {
    if(sum(copycat.hand$Value) <= 10) {
      card$Value[card$Face == "Ace"] <- 11
    } else {
      card$Value[card$Face == "Ace"] <- 1
    }
  } else {
    # dis lelijk
    print("")
  }

  copycat.hand <<- rbind(copycat.hand, dealCard(deck))

  if (sum(copycat.hand$Value) > 17) {
    break;
  }
}
