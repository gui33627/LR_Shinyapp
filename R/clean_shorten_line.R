
clean_shorten_line <- function(first, second, offset){
  
    k <- (first$y[2]-first$y[1])/(first$x[2] - first$x[1])
    b <- first$y[1] + offset - k * first$x[1]
    if(length(which(second$y > 10)) != 0 ){
      idx <- which(second$y > 10)
      second$x[idx] <- (10 - b)/k
      second$y[idx] <- 10
    }
    if(length(which(second$y < 0)) != 0 ){
      idx <- which(second$y < 0)
      second$x[idx] <- (0 - b)/k
      second$y[idx] <- 0
    }
    return(second)
}