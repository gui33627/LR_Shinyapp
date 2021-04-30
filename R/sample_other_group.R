

sample_othe_group <- function(data, module2 = FALSE, diff = NULL){
  
  if(module2 == TRUE){
    
    model <- lm(y~x, data = data)
    epsilon <- rnorm(nrow(data), 0, summary(model)$sigma )
    while (abs(mean(epsilon)) > 0.1 ) {
      epsilon <- rnorm(nrow(data), 0, summary(model)$sigma )
    }
    data_new <- data.frame(x=seq(from = range(data$x)[1], to = range(data$x)[2], length.out = nrow(data))) %>% 
      mutate( fitted.values = summary(model)$coefficients[1] + diff + summary(model)$coefficients[2]*x ,
              y = fitted.values + epsilon)
  }else{
    model <- lm(y~x, data = data)
    epsilon <- rnorm(nrow(data), 0, summary(model)$sigma )
    while (abs(mean(epsilon)) > 0.1 ) {
      epsilon <- rnorm(nrow(data), 0, summary(model)$sigma )
    }
    data_new <- data.frame(x=seq(from = range(data$x)[1], to = range(data$x)[2], length.out = nrow(data))) %>% 
      mutate( fitted.values = summary(model)$coefficients[1] + summary(model)$coefficients[2]*x,
              y = fitted.values + epsilon)
  }

  return(data_new)
  
}