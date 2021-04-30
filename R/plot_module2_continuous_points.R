

plot_module2_continuous_pointsn <- function(data_current, formula, sigma = NULL){
  
  
  if(nrow(data_current) >= 3){
    colors <- c("Regression line" = "brown", "Your points" = "black", "error" = "red" )
    p <- ggplot(data_current) + geom_point(aes(x, y, color = "Your points")) + 
      geom_line(aes(x = x, y = fitted.values, color = "Regression line"), inherit.aes = F, size = 1) + 
      geom_ribbon(aes(x =x, ymin=ci_lwr, ymax=ci_upr), alpha=0.2) +  expand_limits(x = 0, y = 0) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() + 
      geom_segment(aes(x=x, xend=x, y=y, yend=fitted.values, color="error")) +
      geom_text(aes(x=x-0.15, y=(y+fitted.values)/2,  label = error)) +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      annotate("text",x=8, y = 0.5, label = TeX(sigma) ) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
  }else{
    p <- ggplot(data_current, aes(x, y)) + geom_point() + 
      geom_line(aes(x = x, y = fitted.values), inherit.aes = F, color = "brown", size = 1) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) 
  }
  return(p)
  
}