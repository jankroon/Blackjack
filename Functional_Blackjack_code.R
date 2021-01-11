source("userInputFunctions.R")

Blackjack <- function(){
  
# game config
  game_config <- function(){
    # set starting bankroll
    starting_bankroll <<- askForInt("Insert starting bankroll", 10, Inf)

    # set number of players
    number_of_player <<- askForInt("Insert number of physical players", 1, 8)

    #!  set bank?
    PLAYER <<- data.frame(player=c("Bank"), bankroll= as.numeric(Inf), virtual = TRUE)

    # set players name
    for (i in 1:number_of_player) {
      PLAYER[i+1,1] <<- readline("Insert a name : ")
      PLAYER[i+1,2] <<- starting_bankroll
      PLAYER[i+1,3] <<- FALSE
    }

    #enable bots
    enable_bots <<- askForYN("Enable bots")
    if(enable_bots) {
      PLAYER <<- rbind(PLAYER, data.frame(player=c("copycat", "randomrat", "riskbot", "cowardbot"), bankroll=starting_bankroll, virtual=TRUE ))
    } else {
      NULL
    }

    # set number of decks
    number_of_decks <<- askForInt("Insert number of decks", 1, 10)

    # confirming setting  
    confirm_player_choice <<- askForYN("Do you want to continue with these settings")
  }

  # create new game
  new_game <- function(){

    minimum_bet <- as.numeric(readline("Insert minimum bet   "))
    HANDS <<- data.frame("cards"= rep("nothing", times = 1 * nrow(PLAYER)), 
                         player= PLAYER$player, 
                         bet= c(as.numeric(Inf), rep(0, times = 1 * (nrow(PLAYER)-1))),
                         score= rep(0, times = 1 * nrow(PLAYER)),
                         virtual=PLAYER$virtual)

    for (i in 2:nrow(HANDS)){
      repeat {
        repeat{
          if(HANDS$virtual[i] == TRUE) {
            HANDS$bet[i] <<- runif(1, minimum_bet, (minimum_bet * 100))
          } else {
            HANDS$bet[i] <<- as.numeric(readline(paste("Place your bet,", HANDS$player[i], " " ) ))
          }
          
        if (!is.na(HANDS$bet[i])) {break} 
          
        }
        
        if (HANDS$bet[i]>= minimum_bet) {break} 
        
      }
      
      PLAYER$bankroll[i] <<- PLAYER$bankroll[i] - HANDS$bet[i]
    }
        
    make_cards()
    deal_cards()
    ace_control()
    
    if (21 %in% HANDS$score) {
      check_for_winner()
    }
  } 


  # create new round
  new_round <- function() {
    
    hit <- function() {
      card <<- deck[1,]
      deck <<- deck[-1,] 
    }
    
    repeat {
      print(HANDS)
      complete_data <<- merge(HANDS, PLAYER, sort = FALSE)
      players_alive <- complete_data$player[complete_data$virtual == FALSE & complete_data$score <= 21]
      bots_alive <<- complete_data$player[complete_data$virtual == TRUE & complete_data$score <= 21]
      move <- NULL
      
      if (length(bots_alive) >= 1) {
        for (q in 1:length(bots_alive)) {
          strategy_bank <- function() {
            if (HANDS$score[HANDS$player=="Bank"] < 17) {
              move <<- append(move, "hit")
            } else if (HANDS$score[HANDS$player=="Bank"] > 21) { 
              move <<- append(move, NULL)
            } else {
              move <<- append(move, "pass")
            }
          }

          strategy_copycat <- function() {
              if (HANDS$score[HANDS$player == "copycat"] < 17) {
                move <<- append(move, "hit")
              } else if (HANDS$score[HANDS$player == "copycat"] > 21) { 
                move <<- append(move, NULL)
              } else {
                move <<- append(move, "pass")
              }
          }
          
          strategy_cowardbot <- function() {
            if (HANDS$score[HANDS$player == "cowardbot"] < 14) {
              move <<- append(move, "hit")
            } else if (HANDS$score[HANDS$player == "cowardbot"] > 21) { 
              move <<- append(move, NULL)
            } else {
              move <<- append(move, "pass")
            }
          }
          
          strategy_riskbot <- function() {
            if (HANDS$score[HANDS$player == "riskbot"] < 19) {
              move <<- append(move, "hit")
            } else if (HANDS$score[HANDS$player == "riskbot"] > 21) { 
              move <<- append(move, NULL)
            } else {
              move <<- append(move, "pass")
            }
          }

          strategy_randomrat <- function() {
              if (HANDS$score[HANDS$player == "randomrat"] < 17) {
                move <<- append(move, "hit")
              } else if (HANDS$score[HANDS$player == "randomrat"] > 21) {
                move <<- append(move, NULL)
              } else {
                move <<- append(move, "pass")
              }
          }

          strategy_bank()
          if("copycat" %in% HANDS$player) {
            strategy_copycat()
          }

          if("randomrat" %in% HANDS$player) {
            strategy_randomrat()
          }
          
          if("cowardbot" %in% HANDS$player) {
            strategy_cowardbot()
          }
          
          if("riskbot" %in% HANDS$player) {
            strategy_riskbot()
          }
      
          if (is.null(move)){
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
      if (length(players_alive >= 1)) {
        for (i in 1:length(players_alive)) {
          repeat {
            move <- tolower(readline(paste("What is your move,", players_alive[i], "?  ")))
            if (move == "pass") {
              print(paste(players_alive[i], "chose to", move))
              break
            } else if (move == "hit") {
              print(paste(players_alive[i], "chose to", move))
              hit()
              HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==players_alive[i]))))]]  <<- rbind(HANDS[[1]][[(as.numeric(row.names(subset(HANDS, player==players_alive[i]))))]], card)
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
  }

  # create cards
  make_cards <- function(){
    face <- c("Ace", "King", "Queen", "Jack", "10", "9", "8", "7", "6", "5", "4", "3", "2")
    suit <- c("\U2660", "\U2663", "\U2666", "\U2665")
    value <- c(11, 10, 10, 10, 10, 9, 8, 7, 6, 5, 4, 3, 2)
    
    freshdeck <- data.frame(face= rep(face, times = 4 * number_of_decks ), 
                             suit= rep(suit, times = 13 * number_of_decks ), 
                             value= rep(value, times = 4 * number_of_decks))
    
    deck <<- freshdeck[sample(nrow(freshdeck)),]
  }
  
  # deal cards
  deal_cards <- function() {
    a <- seq(1, (nrow(HANDS)*2), by = 2) 
    b <- seq(2, 1+(nrow(HANDS)*2), by = 2)
    
    for (i in 1:nrow(HANDS)) { 
      HANDS$cards[i] <<- list(deck[a[i]:b[i],])
      HANDS$score[i] <<- sum(HANDS$cards[[c(i,3)]])
    }
    deck <<- deck[-(1:(nrow(HANDS)*2)),]
  }

  # check for winners
  check_for_winner <- function() {
    winner <- HANDS$player[HANDS$score==(max(HANDS$score[HANDS$score<=21]))]
    
    if (length(winner)>=1){
      print(paste("Congratulations, The winner is ", winner ,"!"))
      
      for (i in 1:length(winner)) {
        PLAYER$bankroll[PLAYER$player==winner[i]] <<- PLAYER$bankroll[PLAYER$player==winner[i]] + ( HANDS$bet[HANDS$player==winner[i]]*1.5)
      }
    } else {
      print("It's a draw, nobody wins")
    }
    
    print(HANDS[,c(2,4)])
    
    repeat {
      continue <- tolower(readline("Would you like to continue?  "))
      
      if (continue == "yes") {
        endgame <<- FALSE
        continue_game()
        break
      } else if (continue == "no") {
        endgame <<- TRUE
        break
      } else { 
        print("Choose between yes or no") 
      }
    }
  }
  
  # continue game
  continue_game <- function() {
    new_game()
    new_round()
  }
  
  endgame <<- 0
  
  # check if integer
  is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol
  
  # ace control
  ace_control <<- function(){
    for ( i in 1:nrow(HANDS)){
      if ( "Ace" %in% HANDS[[1]][[i]]$face ){
        HANDS[[1]][[i]][3][HANDS[[1]][[i]][1]=="Ace"][1] <<- 11
        HANDS[[1]][[i]][3][HANDS[[1]][[i]][1]=="Ace"][-1] <<- 1
      }
        HANDS[[4]][[i]] <<- sum(HANDS[[1]][[i]][3])
    } 
  } 

  game_config()
  new_game()

  if (endgame) {
    print("Thank you for playing")
  } else if (endgame==FALSE) {
    new_round()
    if (endgame==TRUE) {
      print("Thank you for playing")
    } else { 
      print("something went wrong with endgame")
    }
  } else {
    print("something went wrong with endgame")
  }
  
##### things to do
  # to do, add more moves
  # to do, add different automated strategies for bots 
  # to do, what to do with person with to low bankroll ? 
}



