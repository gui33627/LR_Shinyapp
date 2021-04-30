
interpretation_cmb <- function(b1, b3, positive1 = TRUE, positive2 = TRUE){
  
  if(positive1 == TRUE & positive2 == TRUE){
    
    text <- paste0("For those observations with d=0, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ", 
                   b1, 
                   " units higher/lower than the other group. For those with d=1, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ",
                   b1 + b3, 
                   " units higher/lower than the other group.")
  }else if(positive1 == FALSE & positive2 == FALSE){
    
    text <- paste0("For those observations with d=0, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ", 
                   abs(b1), 
                   " units lower/higher than the other group. For those with d=1, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ",
                   abs(b1 + b3), 
                   " units lower/higher than the other group.")
    
  }else if(positive1 == TRUE & positive2 == FALSE){
    
    text <- paste0("For those observations with d=0, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ", 
                   b1, 
                   " units higher/lower than the other group. For those with d=1, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ", 
                   abs(b1 + b3), 
                   " units lower/higher than the other group.")
  }else{
    text <- paste0("For those observations with d=0, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ", 
                   abs(b1), 
                   " units lower/higher than the other group. For those with d=1, if you compare two groups of observations and one group is one unit higher/lower than the other with respect to x and they have the same value of d then we would expect that the average outcome in units of that group would be ",
                   b1 + b3, 
                   " units higher/lower than the other group.")
    
  }
  return(text)
}