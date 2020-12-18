# functies worden hier geplaats, zodat main overzichtelijk blijft


#1 initialisation

initialisation <- function() {
  
  numberOfPlayers <- as.numeric(readline("Insert number of players  "))
  numberOfRounds  <- as.numeric(readline("Insert number of rounds   "))
  numberOfBots    <- as.numeric(readline("Insert number of bots     "))
  deckSize        <- as.numeric(readline("Insert number of decks    "))
  startBankroll   <- as.numeric(readline("Insert starting Bankroll  "))
  
  # fill in names of virtual players
  bots <- c("Bank", "CopyCat", "RandomRat")
  
  PLAYER <<- data.frame(name=c("Bank"), bankroll= as.numeric(Inf), virtual_or_physical=11)

  
   for (i in 1:numberOfPlayers){
     
     PLAYER[i+1,1] <<- readline("Insert your name ")
     PLAYER[i+1,2] <<- startBankroll
   }


PLAYER$virtual_or_physical <<- 10
PLAYER$virtual_or_physical[PLAYER$name %in% bots] <<- 11


# 2 generate cards and tables 

  face <- c("Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  suit <- c("spades", "clubs", "diamonds", "hearts")
  value <- c(11, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2)
  
  cardSet <- data.frame(face= rep(face, times = 4 * deckSize), 
                       suit= rep(suit, times = 13 * deckSize), 
                       value= rep(value, times = 4 * deckSize))
  
  deck <<- cardSet[sample(nrow(cardSet)),]
  
#3.1 game loop  (first round, this happens no matter what)
  
HANDS <<- data.frame("list of cards"= rep("nothing", times = 1 * nrow(PLAYER)), 
                      player= rep("", times = 1 * nrow(PLAYER)), 
                      bet= rep(0, times = 1 * nrow(PLAYER)),
                      score= rep(0, times = 1 * nrow(PLAYER)))

HANDS$player <<- PLAYER$name
HANDS$bet[1] <<- as.numeric(Inf) 

for (i in 2:nrow(HANDS)){
  
  HANDS$bet[i] <<- as.numeric(readline(paste("Place your bet,", HANDS$player[i], " " ) ))
  PLAYER$bankroll[i] <<- PLAYER$bankroll[i] - HANDS$bet[i]
}


Dealcards <<- function() {
  a <- seq(1,(nrow(HANDS)*2),by=2) 
  b <- seq(2,1+(nrow(HANDS)*2), by=2)
  
  for (i in 1:nrow(HANDS)) { 
    
    HANDS$list.of.cards[i] <<- list(deck[a[i]:b[i],])
    HANDS$score[i] <<- sum(HANDS$list.of.cards[[c(i,3)]])
    
  }
  deck <<- deck[-(1:(nrow(HANDS)*2)),]
}
Dealcards()


if (21 %in% HANDS$score){
  Winner <-  paste(HANDS$player[HANDS$score==21])
  print(paste("Congratulations, The winner is ", Winner ,"!"))
  
  for (i in 1:length(Winner)) {
    
    PLAYER$bankroll[PLAYER$name==Winner[i]] <<- PLAYER$bankroll[PLAYER$name==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
  }
} 


# 3.2 game loop (next round, now you can choose your strategy)













# end of function   
}













