
interpretation_binary <- function(b1, positive = TRUE){
  
  if(positive == TRUE){
    
    text <- paste0("The group of observations with d=1 has average outcome in units that are ", b1, " units higher than the group where d=0.")
  }else{
    
   text <- paste0("The group of observations with d=1 has average outcome in units that are ", abs(b1), " units lower than the group where d=0.") 
    
  }
  return(text)
}