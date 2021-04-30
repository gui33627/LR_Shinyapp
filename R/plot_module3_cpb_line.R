
plot_module3_cpb_line <- function(data_current, group0 = FALSE, group1 = FALSE, done0 = FALSE, done1 = FALSE, formula,
                                  intercept_assess_draw_cpb = NULL, b1_assess_draw_cpb = NULL, b2_assess_draw_cpb = NULL, data_other = NULL){
  if(done0 == FALSE & done1 == FALSE){
    if(group0 == TRUE & group1 == FALSE){
      
      colors <- c("Your line for group0" = "cyan")
      ggplot(data = data_current) + geom_point(aes(x, y, color = "Your line for group0")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(group0 == FALSE & group1 == TRUE){
      
      colors <- c("Your line for group1" = "orange")
      ggplot(data = data_current) + geom_point(aes(x, y, color = "Your line for group1")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6)+
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(group0 == FALSE & group1 == FALSE){

      ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6)
    }
  }else if(done0 == TRUE & done1 == FALSE){
    if(group0 == TRUE & group1 == FALSE){
    colors <- c("Your line for group0" = "cyan", "Your line for group1" = "orange")
    
    ggplot(data = data_current) + geom_line( aes(x, y, color = "Your line for group0")) + 
      geom_line(data = data_other, aes(x = x, y = y, color = "Your line for group1")) + 
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(group0 == TRUE & group1 == TRUE){
      colors <- c("True line for group1" = "brown", "True line for group0" = "black", "Your line for group0" = "cyan", "Your line for group1" = "orange")
      ggplot(data = data_current) + geom_line( aes(x, y, color = "Your line for group0")) + 
        geom_line(data = data_other, aes(x = x, y = y, color = "Your line for group1")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() + 
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }
    
  }else if(done0 == FALSE & done1 == TRUE){
    
    if(group1 == TRUE & group0 == FALSE){
      colors <- c("Your line for group1" = "orange", "Your line for group0" = "cyan")
      ggplot(data = data_current) + geom_line(aes(x, y, color = "Your line for group1")) + 
        geom_line(data = data_other, aes(x = x, y = y, color = "Your line for group0")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(group1 == TRUE & group0 == TRUE){
      colors <- c("True line for group1" = "brown", "True line for group0" = "black", "Your line for group0" = "cyan", "Your line for group1" = "orange")
      ggplot(data = data_current) + geom_line(aes(x, y, color = "Your line for group1")) + 
        geom_line(data = data_other, aes(x = x, y = y, color = "Your line for group0")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
      
    }
  }
}
