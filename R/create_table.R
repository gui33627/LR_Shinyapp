
create_table <- function(name, true, your,coef = FALSE, para = FALSE){
  
  if(para == TRUE){
    table <- data.frame(parameters = name, 
                        true_parameters = true, 
                        your_parameters = your)
  }
  if(coef == TRUE){
    table <- data.frame(coefficients = name, 
               true_coefficients = true, 
               your_coefficients = your)
  }
  return(table)
}