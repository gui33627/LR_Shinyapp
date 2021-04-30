

plot_module2_cpb_submit <- function(data_group0, data_group1, data_full, formula, sigma, group0 = TRUE){
  
  if(group0 == TRUE){
    colors <- c("Your points for group0" = "blue", "Regression line for group0" = "blue", "Regression line for group1" = "red",
                "Your points for group1" = "red","Sample points for group1" = "red", "error" = "black" )
    p <- ggplot(data = data_group0) + geom_point( aes(x, y, color = "Your points for group0")) +
      geom_line(aes(x = x, y = fitted.values, color = "Regression line for group0"), inherit.aes = F, size = 1) + 
      geom_point(data = data_group1, aes(x, y, color = "Sample points for group1")) +
      geom_line(data = data_group1, aes(x = x, y = fitted.values , color = "Regression line for group1"), inherit.aes = F, size = 1) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() + geom_segment(data = data_full, aes(x=x, xend=x, y=y, yend=fitted.values, color="error")) +
      geom_text(data = data_full, aes(x=x-0.15, y=(y+fitted.values)/2,  label = error)) +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      annotate("text",x=7.7, y = 0.5, label = TeX(sigma) ) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
  }else{
    colors <- c("Sample points for group0" = "blue", "Regression line for group0" = "blue", "Regression line for group1" = "red",
                "Your points for group1" = "red","Your points for group1" = "red", "error" = "black" )
    p <- ggplot(data = data_group1) + geom_point( aes(x, y, color = "Your points for group1")) +
      geom_line(aes(x = x, y = fitted.values, color = "Regression line for group1"), inherit.aes = F, size = 1) + 
      geom_point(data = data_group0, aes(x, y, color = "Sample points for group0")) +
      geom_line(data = data_group0, aes(x = x, y = fitted.values, color = "Regression line for group0"), inherit.aes = F, size = 1) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw()  + geom_segment(data = data_full, aes(x=x, xend=x, y=y, yend=fitted.values, color="error")) +
      geom_text(data = data_full, aes(x=x-0.15, y=(y+fitted.values)/2,  label = error)) +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      annotate("text",x=7.7, y = 0.5, label = TeX(sigma) ) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
  }
  return(p)
}