# decision strategy proposal
# NOT TESTED !!!


player.decision <- function(player.hand, bank.hand){
  # Strategy for beginners  from meneer Casino
  
  visiblebankcard = bank.hand[1] # een kaart is zichtbaar
  vbc = strtoi(visiblebankcard)
  if (is.na(vbc)){vbc = 10} 
  
  laag = FALSE
  hoog = FALSE
  
  if (vbc>=2 && vbc<=6){
    laag = TRUE
  }else{
    hoog = TRUE
  }
  
  
  hv = handValue(player.hand)
  azen = 0
  
  for (row in 1:nrow(player.hand)){
    if (player.hand[row,1] == "Ace"){
      azen = azen + 1
    }
  }
  
  if (hv > 21){return("loss")}
  
  if (azen==0 && laag && hv>=12){
    return("pass")
  }else{
    return("buy")
  }
  
  if (azen==0 && hoog && hv>=17){
    return("pass")
  }else{
    return("buy")
  }
  
  if (azen>0 && laag && hv>=18){
    return("pass")
  }else{
    return("buy")
  }
  
  if (azen>0 && hoog && hv>=19){
    return("pass")
  }else{
    return("buy")
  }
  
  
}