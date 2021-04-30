

plot_module3_cmb_line <- function(group0 = FALSE, group1 = FALSE, data_current = NULL, data_0 = NULL, data_1 = NULL, done0 = FALSE, done1 = FALSE,
                                  intercept_assess_draw_cmb = NULL, b1_assess_draw_cmb = NULL, b2_assess_draw_cmb = NULL, b3_assess_draw_cmb = NULL, formula = NULL){
  
  if(group0 == TRUE & group1 == FALSE){
    if(done0  == FALSE){
    colors <- c("Your line for group0" = "cyan")
    ggplot(data = data_current) + geom_point(aes(x, y, color = "Your line for group0")) + 
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + e", fontface = "italic", size = 6) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else{
      colors <- c("Your line for group0" = "cyan")
      ggplot() + geom_line(data = data_current, aes(x, y, color = "Your line for group0")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }
  }else if(group0 == FALSE & group1 == TRUE){
    if(done1 == FALSE){
    colors <- c("Your line for group1" = "orange")
    ggplot(data = data_current) + geom_point(aes(x, y, color = "Your line for group1")) + 
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6)+
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else{
      colors <- c("Your line for group1" = "orange", "Your line for group0" = "cyan")
      ggplot(data = data_current) + geom_line(aes(x, y, color = "Your line for group1")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }
  }else if(group0 == TRUE & group1 == TRUE){
    if(done0 == TRUE & done1 == FALSE){
      colors <- c("Your line for group1" = "orange", "Your line for group0" = "cyan")
      ggplot() + geom_line(data = data_0, aes(x, y, color = "Your line for group0")) + 
        geom_point(data = data_1, aes(x, y, color = "Your line for group1")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(done0 == FALSE & done1 == FALSE){

    colors <- c("Your line for group1" = "orange", "Your line for group0" = "cyan")
    ggplot() + geom_point(data = data_0, aes(x, y, color = "Your line for group0")) + 
      geom_point(data = data_1, aes(x, y, color = "Your line for group1")) + 
      expand_limits(x = 0, y = 0) +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(done0 == FALSE & done1 == TRUE){
      colors <- c("Your line for group1" = "orange", "Your line for group0" = "cyan")
      ggplot() + geom_point(data = data_0, aes(x, y, color = "Your line for group0")) + 
        geom_line(data = data_1, aes(x, y, color = "Your line for group1")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else{
      
      colors <- c("True line for group1" = "brown", "True line for group0" = "black", "Your line for group0" = "cyan", "Your line for group1" = "orange")
      ggplot() + geom_line(data = data_0, aes(x, y, color = "Your line for group0")) + 
        geom_line(data = data_1, aes(x, y, color = "Your line for group1")) + 
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw_cmb, slope = b1_assess_draw_cmb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cmb + b2_assess_draw_cmb, slope = b1_assess_draw_cmb + b3_assess_draw_cmb, color = "True line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }
  }else{
    ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6)
  }
}