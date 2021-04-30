
clean_to_dataframe <- function(data){
  df <- as.data.frame(matrix(data,ncol = 2, byrow = T))
  colnames(df) <- c("x","y")
  return(df)
}