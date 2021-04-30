
clean_specified_size <- function(data_current, size){
  if(nrow(data_current) >= size){
    data_current <- data_current[1:size,]
  }
  return(data_current)
}