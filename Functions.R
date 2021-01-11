# functies worden hier geplaats, zodat main overzichtelijk blijft


#1 initialisation

initialisation <- function() {

  
### 
  ask_if_new_game <- function(){
    skip <- 1
    while (skip==1){
      if ((skip <-readline("do you want to continue? yes/no   ")) =="yes"){
        new_round()
        if ( skip!=1){ break }
      } else if (skip == "no") {
        print("Thank you for playing.")
        if (skip!=1){ break }
      }  else {
        skip <- 1
        print("Choose between yes or no")
      }
    }
  }
###
  endgame <- function(){
    
    if (readline("do you want to continue? yes/no   ") =="yes"){
         new_round()
              } else {
          print("Thank you for playing.")
        }
  }
#####
  one_round <- function(){
    
    if (21 %in% HANDS$score){
      Winner <-  paste(HANDS$player[HANDS$score==21])
          print(paste("Congratulations, The winner is ", Winner ,"!"))
      
                for (i in 1:length(Winner)) {
        
           PLAYER$bankroll[PLAYER$name==Winner[i]] <<- PLAYER$bankroll[PLAYER$name==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
       print(HANDS)
           
      ask_if_new_game()
      }
    } 
    
    print(PLAYER)
    # 3.2 game loop (next round, now you can choose your strategy)
    
    # available moves
    hit <- function() {
      
      card <<- deck[1,]  
      deck <<- deck[-1,]
    } 
    
    physical_player <- PLAYER$name[PLAYER$virtual_or_physical==10]
    
    
    while (current_round < numberOfRounds ) { 
      
      current_round <- current_round + 1
      writeLines((paste("Round", current_round)))
      print(HANDS)
      
      
      for ( i in 1:length(physical_player)){
        
        repeat{
          
          move  <-  readline(paste("What is your move,", physical_player[i], "?  "))
          
          if (move == "pass"| move=="Pass" ){
            print(paste(physical_player[i], "chose to",move))
            break
            
          } else if (move == "hit"| move== "Hit"){
            print(paste(physical_player[i], "chose to",move))
            hit()
            HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==physical_player[i]))))]]  <<- rbind(HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==physical_player[i]))))]], card)
            HANDS$score[HANDS$player==physical_player[i]] <<- sum(HANDS$list.of.cards[[c((as.numeric(row.names(subset(HANDS, player==physical_player[i])))),3)]])
            break
            
          } else {
            print("Non-existing move, did you spell it correctly?")
          }
        }
      }
    }
    
    
    Winner <-  HANDS$player[HANDS$score==max(HANDS$score[HANDS$score<22])]
    for ( i in 1:length(Winner)){
      print(paste("Congratulations, The winner is ", HANDS$player[HANDS$player==Winner[i]] ,"!"))
    }
    
    # adding to their bankroll
    for (i in 1:length(Winner)) {
      
      PLAYER$bankroll[PLAYER$name==Winner[i]] <<- PLAYER$bankroll[PLAYER$name==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
    }
}
  
  
  numberOfPlayers <- as.numeric(readline("Insert number of players  "))
  numberOfRounds  <- as.numeric(readline("Insert number of rounds   "))
  #numberOfBots    <- as.numeric(readline("Insert number of bots     "))
  deckSize        <- as.numeric(readline("Insert number of decks    "))
  startBankroll   <- as.numeric(readline("Insert starting Bankroll  "))
  
  # fill in names of virtual players
  bots <- c("Bank", "CopyCat", "RandomRat")
  
  PLAYER <<- data.frame(name=c("Bank"), bankroll= as.numeric(Inf), virtual_or_physical=11)

  
  for (i in 1:numberOfPlayers){
     
         PLAYER[i+1,1] <<- readline("Insert your name ")
        PLAYER[i+1,2] <<- startBankroll
}
###
  new_round <- function(){
    
    new_shuffled_deck()
    Dealcards()
    insertbets()
    one_round()
    endgame()
    # start round 
  }
###

PLAYER$virtual_or_physical <<- 10
PLAYER$virtual_or_physical[PLAYER$name %in% bots] <<- 11


# 2 generate cards and tables 


  face <- c("Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6", "5", "4", "3", "2")
  suit <- c("spades", "clubs", "diamonds", "hearts")
  value <- c(11, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2)
  
  cardSet <<- data.frame(face= rep(face, times = 4 * deckSize), 
                       suit= rep(suit, times = 13 * deckSize), 
                       value= rep(value, times = 4 * deckSize))
  
new_shuffled_deck <<- function(){
  deck <<- cardSet[sample(nrow(cardSet)),]
}
new_shuffled_deck()
#3.1 game loop  (first round, this happens no matter what)
current_round <- 1
  
HANDS <<- data.frame("list of cards"= rep("nothing", times = 1 * nrow(PLAYER)), 
                      player= PLAYER$name, 
                      bet= c(as.numeric(Inf), rep(0, times = 1 * (nrow(PLAYER)-1))),
                      score= rep(0, times = 1 * nrow(PLAYER)))


insertbets <- function(){
for (i in 2:nrow(HANDS)){
  
  HANDS$bet[i] <<- as.numeric(readline(paste("Place your bet,", HANDS$player[i], " " ) ))
  PLAYER$bankroll[i] <<- PLAYER$bankroll[i] - HANDS$bet[i]
}
}
insertbets()

# first bank card must become invisible. idea: make 2 df, 1 for data & 1 visible for players
# BETTER IDEA, PUT THE FIRST CARD IN A SEPERATE DF AND MAKE HANDS$SCORE ADD UP FROM THAT DF 
Dealcards <- function() {
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
    print(HANDS)
    ask_if_new_game()
    
  }
} 

print(PLAYER)
# 3.2 game loop (next round, now you can choose your strategy)

# available moves
hit <<- function() {
  
  card <<- deck[1,]  
  deck <<- deck[-1,]
} 

physical_player <- PLAYER$name[PLAYER$virtual_or_physical==10]


while (current_round < numberOfRounds ) { 

  current_round <- current_round + 1
  writeLines((paste("Round", current_round)))
  print(HANDS)
  
  
  for ( i in 1:length(physical_player)){
    
    repeat{
      
      move  <-  readline(paste("What is your move,", physical_player[i], "?  "))
      
      if (move == "pass"| move=="Pass" ){
        print(paste(physical_player[i], "chose to",move))
        break
        
      } else if (move == "hit"| move== "Hit"){
        print(paste(physical_player[i], "chose to",move))
        hit()
        HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==physical_player[i]))))]]  <<- rbind(HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==physical_player[i]))))]], card)
        HANDS$score[HANDS$player==physical_player[i]] <<- sum(HANDS$list.of.cards[[c((as.numeric(row.names(subset(HANDS, player==physical_player[i])))),3)]])
        break
        
      } else {
        print("Non-existing move, did you spell it correctly?")
      }
    }
  }
}


Winner <-  HANDS$player[HANDS$score==max(HANDS$score[HANDS$score<22])]
for ( i in 1:length(Winner)){
  print(paste("Congratulations, The winner is ", HANDS$player[HANDS$player==Winner[i]] ,"!"))
}

# adding to their bankroll
  for (i in 1:length(Winner)) {
    
    PLAYER$bankroll[PLAYER$name==Winner[i]] <<- PLAYER$bankroll[PLAYER$name==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
  }

print(HANDS)
print("The game has ended.")


# to do, fix situation with aces
# add strategies to bot players
# make HANDS more clear

ask_if_new_game()


  

# end of function   
}













