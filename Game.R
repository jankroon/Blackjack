# Game loop for multiple Blackjack player and the bank

source("RandomRat.R")

# Test Cases
RandomRat.bankroll <- 50000

print(paste("RandomRat bets: ", bet(RandomRat.bankroll)))
print(paste("RandomRat turn: ", turn(0, 0)))
