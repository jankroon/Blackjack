Blackjack <- function(){
  
###### object 1
make_cards <- function(){
    face <- c("Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6", "5", "4", "3", "2")
    suit <- c("spades", "clubs", "diamonds", "hearts")
    value <- c(11, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2)
    
    freshdeck <<- data.frame(face= rep(face, times = 4 ), 
                             suit= rep(suit, times = 13 ), 
                             value= rep(value, times = 4))
    deck <<- freshdeck[sample(nrow(freshdeck)),]} 
##### object 2
deal_cards <- function() {
  a <- seq(1,(nrow(HANDS)*2),by=2) 
  b <- seq(2,1+(nrow(HANDS)*2), by=2)
  
  for (i in 1:nrow(HANDS)) { 
    
    HANDS$cards[i] <<- list(deck[a[i]:b[i],])
    HANDS$score[i] <<- sum(HANDS$cards[[c(i,3)]])
    
  }
  deck <<- deck[-(1:(nrow(HANDS)*2)),]
}
#### object 3
check_for_blackjack <- function(){
  if (21 %in% HANDS$score){
    Winner <-  paste(HANDS$player[HANDS$score==21])
    print(paste("Congratulations, The winner is ", Winner ,"!"))
    for (i in 1:length(Winner)) {
      
      PLAYER$bankroll[PLAYER$player==Winner[i]] <<- PLAYER$bankroll[PLAYER$player==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
    }
    print(HANDS[,c(2,4)])
    repeat{
      continue <- tolower(readline("Would you like to continue?  "))
      
      if (continue=="yes") {
        continue_game()
        break
      } else if (continue=="no"){
        print("Thank you for playing")
        break
      } else { print("Choose between yes or no") }
    } 
  } 
}
### object 4
check_for_winner <- function(){
  if (21 %in% HANDS$score){
    Winner <-  paste(HANDS$player[HANDS$score==21])
    print(paste("Congratulations, The winner is ", Winner ,"!"))
    for (i in 1:length(Winner)) {
      
      PLAYER$bankroll[PLAYER$player==Winner[i]] <<- PLAYER$bankroll[PLAYER$player==Winner[i]] + ( HANDS$bet[HANDS$player==Winner[i]]*1.5)
    }
    print(HANDS[,c(2,4)])
    repeat{
      continue <- tolower(readline("Would you like to continue?  "))
      
      if (continue=="yes") {
        continue_game()
        break
      } else if (continue=="no"){
        print("Thank you for playing")
        break
      } else { print("Choose between yes or no") }
    } 
  } 
}
## object 5

#

start_game <- function(){

  starting_bankroll <- as.numeric(readline("Insert starting Bankroll  "))
  PLAYER <<- data.frame(player=c("Bank"), bankroll= as.numeric(Inf), virtual_or_physical=11)
    number_of_player <- as.numeric(readline("Insert number of physical players   "))
    for ( i in 1:number_of_player){
      PLAYER[i+1,1] <<- readline("Insert your name  ")
      PLAYER[i+1,2] <<- starting_bankroll
      PLAYER[i+1,3] <<- 10 }
 
  repeat{
      enable_bots <- tolower(readline("Enable bots? yes/no  "))
    
   if (enable_bots=="yes") {
      PLAYER <<- rbind(PLAYER, data.frame(player=c("copycat", "randomrat"), bankroll=starting_bankroll, virtual_or_physical=11 ))
      break
    } else if (enable_bots=="no"){ 
      break
    } else { print("Choose between yes or no") }
  }
}# end of start_game
start_game()



new_game <- function(){
  
  minimum_bet <- as.numeric(readline("Insert minimum bet   "))
  HANDS <<- data.frame("cards"= rep("nothing", times = 1 * nrow(PLAYER)), 
                       player= PLAYER$player, 
                       bet= c(as.numeric(Inf), rep(0, times = 1 * (nrow(PLAYER)-1))),
                       score= rep(0, times = 1 * nrow(PLAYER)))
  
  for (i in 2:nrow(HANDS)){
    repeat {
    HANDS$bet[i] <<- as.numeric(readline(paste("Place your bet,", HANDS$player[i], " " ) ))
    if (HANDS$bet[i]>= minimum_bet)
    {break} }
    PLAYER$bankroll[i] <<- PLAYER$bankroll[i] - HANDS$bet[i]
  }
  
  make_cards()
  deal_cards()

repeat {
  if (21 %in% HANDS$score){
  check_for_winner()
  }
  break
}  
  
} #end of new_game
new_game()

# checking for winner if someone has 21 at the bat


#####work in progresss test verison !!!!
# to do, end when everyone passes or is dead
# to do, add more moves
# to do, add automated strategies for bots 
# to do, what to do with person with to low bankroll ? 
# ace control ?
# continue "no" not workring correctly

new_round <<- function() {
  
  hit <- function() {
    card <<- deck[1,]
    deck <<- deck[-1,] }


  repeat{
    print(HANDS)
    complete_data <<- merge(HANDS, PLAYER, sort = FALSE)
    players_alive <- complete_data$player[complete_data$virtual_or_physical==10 & complete_data$score<=21]
    
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
          HANDS$score[HANDS$player==players_alive[i]] <<- sum(HANDS$cards[[c((as.numeric(row.names(subset(HANDS, player==players_alive[i])))),3)]])
          break
        } else {
          print("Non-existing move, did you spell it correctly?")
        }
      }
    }
   # break if player dies or everyone passes
    if (all(complete_data$score==HANDS$score)==TRUE){
      break
      
    }
  
      
  }
  
check_for_winner()

} # end of new_round

new_round()
 
  
  

  
  
  #end of blackjack
}

