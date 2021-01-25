# pretty print for HANDS
# breaks if HANDS gets another structure
# use "all" for all players or c(....) for a selection i.e. c(1,2,3)

printHand <- function(hand,selection){
  
  if (any(selection=="all")){
    selection = c(1:nrow(hand))
  }
  
  for (i in selection){
    
    player = paste(hand[i,2],":             ")
    out = substr(player,1,14)
    cards = hand[i,1][[1]]
    points = hand[i,4]
    pntstr = toString(points)
    
    if (points < 10){
      pntstr = paste(pntstr," ")
    } 
    
    cardout = ""
    
    for (c in 1:nrow(cards)){
      
      
      cardout = paste(cardout,cards[c,2],substr(cards[c,1],1,1),"")
      
      
      
    }
    
    out = paste(out,cardout,"->",pntstr,"pnts.\n")
    cat(out)
    
  }
  
  
  
}



