

plot_module3_continuous_line <- function(data_current, done = FALSE, formula = NULL, intercept_assess_draw = NULL, coefficient_assess_draw = NULL){
  if(done == FALSE){
    
    if(nrow(data_current) != 0){
      ggplot(data = data_current, aes(x, y)) + geom_point() + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
    }else{
      ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
    } 
  }else{
    colors <- c("Your line" = "brown", "True line" = "black")
    ggplot(data = data_current, aes(x, y)) + geom_point() + 
      geom_line(aes(x = x, y = y, color = "Your line"), inherit.aes = F, size = 1) +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_abline(aes(intercept = intercept_assess_draw, slope = coefficient_assess_draw, color = "True line"), show.legend = FALSE) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
  }
 
}