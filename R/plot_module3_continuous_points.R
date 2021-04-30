
plot_module3_continuous_points <- function(data_current, formula,intercept_assess_draw, coefficient_assess_draw, samplesize = FALSE){
  
  if(nrow(data_current) != 0){
    if(samplesize==TRUE){
      colors <- c("Your line" = "brown", "True line" = "black")
      p <- ggplot(data_current) + geom_point(aes(x, y)) + 
        geom_line(data = data_current, aes(x = x, y = fitted.values, color = "Your line"), inherit.aes = F, size = 1) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw, slope = coefficient_assess_draw, color = "True line"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(samplesize==FALSE){
      
      p <- ggplot(data_current, aes(x, y)) + geom_point() + 
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(intercept = intercept_assess_draw, slope = coefficient_assess_draw)
    }
  }else{
    p <- ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_abline(intercept = intercept_assess_draw, slope = coefficient_assess_draw)
  }
  return(p)
}