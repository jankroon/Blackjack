# useful functions for user input

# check if integer
is.wholenumber <- function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol


askForInt <- function(question, minValue, maxValue){
  
  # ask for integer between minValue and maxValue
  # example: test <- askForInt("What is your age",10,100)

  error = '\U26A0'
  
  repeat {
    result <- as.numeric(readline(paste(question,"? : ")))
    
    if (is.na(result)) {
      cat(paste(error,"Must be a numeric value"))
    } else if (!is.wholenumber(result)) {
      cat(paste(error,"Must be an integer value"))
    } else if ( result <= minValue) {
      cat(paste(error,"Value must be higer than", toString(minValue)))
    } else if (result >= maxValue) {
      cat(paste(error,"Value must be lower than", toString(maxValue)))
    } else {
      break
    }
  }
    return(result)
  }
  

askForYN <- function(question){
  # asks for yes or no (capitals allowed, dutch allowed)
  # returns a boolean
  # example: test <- askForYN("Continue game")

  error = '\U26A0'
  
  repeat {
  result <- tolower(readline(paste(question,"? y(es)/n(o):")))
  
  if (result %in% c("yes","y","ja","j")){
    return(TRUE)
  } else if (result %in% c("no","n","nee")){ 
    return(FALSE)
  } else { 
    cat(paste(error,"Choose between y(es) or n(o)"))
  }
  }
}


