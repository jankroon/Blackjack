Blackjack <- function(){
  
# functie 1
start_game <- function(){
    
    
  repeat {
    starting_bankroll <- as.numeric(readline("Insert starting bankroll  "))
    if ((is.na(starting_bankroll))==TRUE){
        print("Choose a numeric value")
   } else if ((is.wholenumber(starting_bankroll))==FALSE){
        print("Bankroll must be an integer value")
   } else if ( starting_bankroll<=10){
        print("Bankroll must be higer than 10 ")
   }  else {
        break
   }
  }

    PLAYER <<- data.frame(player=c("Bank"), bankroll= as.numeric(Inf), virtual_or_physical=11)
    
    # inserting number of players
    repeat {
      number_of_player <- as.numeric(readline("Insert number of physical players   "))
      if ((is.na(number_of_player))==TRUE){
        print("Choose a numeric value")
      } else if (is.wholenumber(number_of_player)==FALSE){
        print("Number of players must be an integer value")
      } else if (number_of_player<1){
        print("Minimum of 1 player is required")
      } else if (number_of_player>8){
        print("Maximum of 8 players is allowed")
      } else {
        break
      }
    }

    # inserting players
    for ( i in 1:number_of_player){
      PLAYER[i+1,1] <<- readline("Insert your name  ")
      PLAYER[i+1,2] <<- starting_bankroll
      PLAYER[i+1,3] <<- 10 
      }
    
    repeat{
      enable_bots <- tolower(readline("Enable bots? yes/no  "))
      if (enable_bots=="yes") {
        PLAYER <<- rbind(PLAYER, data.frame(player=c("copycat", "randomrat"), bankroll=starting_bankroll, virtual_or_physical=11 ))
        break
      } else if (enable_bots=="no"){ 
        break
      } else { 
        print("Choose between yes or no") 
      }
    }
  
    # inserting deck
    repeat {
      number_of_decks <<- as.numeric(readline("Insert number of decks  "))
      if ((is.na(number_of_decks))==TRUE){
          print("Choose a numeric value")
      } else if ( number_of_decks>10){
          print("Maximum of 10 decks is allowed")
      } else if ( number_of_decks<1){
          print("Minimum of 1 deck is required")
      } else if (is.wholenumber(number_of_decks)==FALSE){
          print("Deck must be an integer value")
      } else {
           break
      }
    }
    
  # confirming setting  
  repeat {
  confirm_player_choice <- tolower(readline("Do you want to continue with these settings?.. yes/no  "))
  
    if (confirm_player_choice=="yes"){
        print("lets play !")
        break
    } else if ( confirm_player_choice=="no"){
        print("Change your settings")
      start_game()
      break
    } else {
        print("Choose between yes or no...")
    } 
  }
  

    
  }# end of start_game
  

# functie 2
new_game <- function(){
    
    minimum_bet <- as.numeric(readline("Insert minimum bet   "))
    HANDS <<- data.frame("cards"= rep("nothing", times = 1 * nrow(PLAYER)), 
                         player= PLAYER$player, 
                         bet= c(as.numeric(Inf), rep(0, times = 1 * (nrow(PLAYER)-1))),
                         score= rep(0, times = 1 * nrow(PLAYER)))
    
    for (i in 2:nrow(HANDS)){
      repeat {
        repeat{
        HANDS$bet[i] <<- as.numeric(readline(paste("Place your bet,", HANDS$player[i], " " ) ))
        if ( (is.na(HANDS$bet[i]))==FALSE ){ break} }
        if (HANDS$bet[i]>= minimum_bet)
        {break} }
      PLAYER$bankroll[i] <<- PLAYER$bankroll[i] - HANDS$bet[i]
    }
        
  make_cards()
  deal_cards()
  ace_control()
    
    if (21 %in% HANDS$score){
      check_for_winner()
      }
    
  } #end of new_game


# functie 3
new_round <- function() {
    
    hit <- function() {
      card <<- deck[1,]
      deck <<- deck[-1,] }
    
    repeat{
      
      print(HANDS)
      complete_data <<- merge(HANDS, PLAYER, sort = FALSE)
      players_alive <- complete_data$player[complete_data$virtual_or_physical==10 & complete_data$score<=21]
      bots_alive <- complete_data$player[complete_data$virtual_or_physical==11 & complete_data$score<=21]
      move <- NULL
      
  if (length(bots_alive)>=1){
      for ( q in 1:length(bots_alive)){
        
    strategy_bank <- function(){
          
          if (HANDS$score[HANDS$player=="Bank"]<17){
            move <<- append(move, "hit")
          } else if (HANDS$score[HANDS$player=="Bank"]>21){ 
            move <<- append(move, NULL)
          } else {
            move <<- append(move, "pass")
          }
        }
        
    strategy_copycat <- function(){
          
          if (HANDS$score[HANDS$player=="copycat"]<17){
            move <<- append(move, "hit")
          } else if (HANDS$score[HANDS$player=="copycat"]>21){ 
            move <<- append(move, NULL)
          } else {
            move <<- append(move, "pass")
          }
        }
        
    strategy_randomrat <- function(){
          
          if (HANDS$score[HANDS$player=="randomrat"]<17){
            move <<- append(move, "hit")
          } else if (HANDS$score[HANDS$player=="randomrat"]>21){ 
            move <<- append(move, NULL)
          } else {
            move <<- append(move, "pass")
          }
        }
      strategy_bank()
      strategy_copycat()
      strategy_randomrat()
      
      if (is.null(move)==TRUE){
        print("Bots are out")  
      } else if (move[q] == "pass"){
        print(paste(bots_alive[q], "chose to",move[q]))
      } else if (move[q] == "hit"){
        print(paste(bots_alive[q], "chose to",move[q]))
        hit()
        HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==bots_alive[q]))))]]  <<- rbind(HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==bots_alive[q]))))]], card)
        ace_control()
      }
    }
  }
      
      
      #in progress start 
    if (length(players_alive>=1)){
      for ( i in 1:length(players_alive)){
        repeat{
          move  <-  tolower(readline(paste("What is your move,", players_alive[i], "?  ")))
          if (move == "pass"){
            print(paste(players_alive[i], "chose to",move))
            break
          } else if (move == "hit"){
            print(paste(players_alive[i], "chose to",move))
            hit()
            HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==players_alive[i]))))]]  <<- rbind(HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==players_alive[i]))))]], card)
            #HANDS$score[HANDS$player==players_alive[i]] <<- sum(HANDS$cards[[c((as.numeric(row.names(subset(HANDS, player==players_alive[i])))),3)]])
            ace_control()
            break
          } else {
            print("Non-existing move, did you spell it correctly?")
          }
        }
      }
    }
      

      # break if player dies or everyone passes
      if ( (all( complete_data$score == HANDS$score )) ==TRUE){
        break
        
      }
    }
    
    check_for_winner()
    
  } # end of new_round
 

# LOSSE OBJECTEN
  
 ###### object 1
  make_cards <- function(){
    face <- c("Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6", "5", "4", "3", "2")
    suit <- c("spades", "clubs", "diamonds", "hearts")
    value <- c(11, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2)
    
    freshdeck <- data.frame(face= rep(face, times = 4 * number_of_decks ), 
                             suit= rep(suit, times = 13 * number_of_decks ), 
                             value= rep(value, times = 4 * number_of_decks))
    
    deck <<- freshdeck[sample(nrow(freshdeck)),]
    } # end of make_cards 
  
 ##### object 2
  deal_cards <- function() {
    a <- seq(1,(nrow(HANDS)*2),by=2) 
    b <- seq(2,1+(nrow(HANDS)*2), by=2)
    
    for (i in 1:nrow(HANDS)) { 
      
      HANDS$cards[i] <<- list(deck[a[i]:b[i],])
      HANDS$score[i] <<- sum(HANDS$cards[[c(i,3)]])
      
    }
    deck <<- deck[-(1:(nrow(HANDS)*2)),]
  } # end of deal_cards
  

 ### object 3
  check_for_winner <- function(){
    
    Winner <- HANDS$player[HANDS$score==(max(HANDS$score[HANDS$score<=21]))]
    if (length(Winner)>=1){
    print(paste("Congratulations, The winner is ", Winner ,"!"))
    
    for (i in 1:length(Winner)) {
      
      PLAYER$bankroll[PLAYER$player==Winner[i]] <<- PLAYER$bankroll[PLAYER$player==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
    }
    } else {
      print("It's a draw, nobody wins")
    }
    print(HANDS[,c(2,4)])
    repeat{
      continue <- tolower(readline("Would you like to continue?  "))
      
      if (continue=="yes") {
        endgame <<- FALSE
        continue_game()
        break
      } else if (continue=="no"){
        endgame<<- TRUE
        break
      } else { 
        print("Choose between yes or no") }
    }
    
  } # end of check_for_winner
  
 ## object 4
  continue_game <- function() {
    new_game()
    new_round()
  }
 # object 5
  endgame <<- 0
 # object 6
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  # object 7 ace control
  ace_control <<- function(){
    for ( i in 1:nrow(HANDS)){
      if ( "Ace" %in% HANDS[[1]][[i]]$face ){
        
        HANDS[[1]][[i]][3][HANDS[[1]][[i]][1]=="Ace"][1] <<- 11
        HANDS[[1]][[i]][3][HANDS[[1]][[i]][1]=="Ace"][-1] <<- 1
      }
        HANDS[[4]][[i]] <<- sum(HANDS[[1]][[i]][3])
      
    } 
  } 
  
  
# HERE STARTS THE EXECUTION LINES
  
  
start_game()
  
  
new_game()

if (endgame==TRUE){
  print("Thank you for playing")
  } else if (endgame==FALSE) {
    
       new_round()
       if (endgame==TRUE){
          print("Thank you for playing")
      } else { 
         "something went wrong with endgame"}
    
  } else {
    print("something went wrong with endgame")
  }
  
#####work in progresss test verison !!!!
  # to do, add more moves
  # to do, add automated strategies for bots 
  # to do, what to do with person with to low bankroll ? 
#### ace control ?


  
  
  #end of blackjack
}



