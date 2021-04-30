
interpretation_continuous <- function(b1, positive  = TRUE){
  
  if(positive == TRUE){
    text <- paste0("If you compare two groups of observations and one group is one unit higher/lower than the other with respect to x then we would expect that the average outcome in units of that group would be ", 
                   b1, " units higher/lower than the other group.")
  }else{
    text <- paste0("If you compare two groups of observations and one group is one unit higher/lower than the other with respect to x then we would expect that the average outcome in units of that group would be ", 
                   abs(b1), " units lower/higher than the other group.")
  }
  return(text)
}