
plot_module4_continuous <- function(data_true, line = FALSE, submit = FALSE, formula = NULL, b0 = NULL, b1 = NULL){
  
  if(line == TRUE){
    
    if(submit == TRUE){
      colors <- c("Line of the coefficients of your choice" = "cyan", "Line of the coefficients in the question" = "black")
      ggplot(data = data_true, aes(x, y)) +
        geom_line(aes(x = x, y = y, color = "Line of the coefficients in the question"), inherit.aes = F, size = 1) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = b0, slope = b1, color = "Line of the coefficients of your choice"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }else{
      ggplot(data = data_true, aes(x, y)) +
        geom_line(aes(x = x, y = y), inherit.aes = F, color = "black", size = 1) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
    }
  }else{
    if(submit == TRUE){
      colors <- c("Obs. with sigma of your choice" = "blue", "Obs. with sigma in the question" = "black")
      ggplot(data_true) + geom_point(aes(x, y_sample, color = "Obs. with sigma in the question")) + 
        geom_line(data = data_true, aes(x = x, y = y), inherit.aes = F, color = "black", size = 1) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) + 
        geom_point(aes(x, y_sample_user, color = "Obs. with sigma of your choice")) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }else{
      ggplot(data_true, aes(x, y_sample)) + geom_point() + 
        geom_line(data = data_true, aes(x = x, y = y), inherit.aes = F, color = "black", size = 1) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
    }
    
  }

}