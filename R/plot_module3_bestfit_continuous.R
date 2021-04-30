
plot_module3_bestfit_continuous <- function(data_current, data_true, formula = NULL){
  
  if(nrow(data_current) == 2){
    colors <- c("Your line" = "cyan", "True line" = "black")
    ggplot(data = data_true) + geom_point(aes(x, y_sample)) + 
      geom_line(aes(x = x, y = y, color = "True line"), inherit.aes = F, size = 1) +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_line(data = data_current, aes(x = x, y = y, color = "Your line"), show.legend = FALSE) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
  }else if(nrow(data_current) == 0){
    ggplot(data = data_true) + geom_point(aes(x, y_sample)) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
  }else{
    ggplot(data = data_true) + geom_point(aes(x, y_sample)) + 
      geom_point(data = data_current, aes(x = x[1], y = y[1]), inherit.aes = F, color = "cyan", size = 1) +
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
  }
}