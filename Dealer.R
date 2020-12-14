# Dealer functions for Blackjack simulation
createDeck <- function() {
  face <- c("Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  suit <- c("spades", "clubs", "diamonds", "hearts")
  value <- c(11, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2)

  deck <<- data.frame(suit = c(), face = c(), value = c())
  for (i in 1:13){
    for (s in suit) {
      deck <<- rbind(deck, list(face[i], s, value[i]))    
    }
    colnames(deck) <<- c("Face", "Suit", "Value")
  }

  return(deck)
}

shuffleDeck <- function(deck) {
  random.order <- sample(1:nrow(deck), size = nrow(deck))
  deck <- deck[random.order, ]
  print("SHUFFLED")
  return(deck)
}

dealCard <- function(deck){
  card <- deck[1, ]
  assign("deck", deck[-1, ], envir=globalenv())
  return(card)
}

createHand <- function() {
  #how to dynamically create a players hand?? 
  return(data.frame(dealCard(deck)))
}
