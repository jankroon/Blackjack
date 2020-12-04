# Player without any strategy

bet <- function(bankroll){
  amount <- sample(1:bankroll, size=1)
  bankroll <- bankroll - amount
  return(amount)
}

turn <- function(bank.hand, my.hand){
  response = sample(c("pass", "hit", "double", "split"), size=1)
  return(response)
}

