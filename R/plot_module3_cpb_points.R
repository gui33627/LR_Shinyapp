
plot_module3_cpb_points <- function(data_current = NULL, data_0 = NULL, data_1 = NULL, group0 = FALSE, group1 = FALSE, intercept_assess_draw_cpb, b1_assess_draw_cpb, b2_assess_draw_cpb, 
                                    formula, submit0 = FALSE, submit1 = FALSE, flag = NULL, data_0_fit = NULL, data_1_fit = NULL){
  if(submit0 == FALSE & submit1 == FALSE){
    if(group0 == TRUE & group1 == FALSE){
      colors <- c("Your points for group0" = "blue", "True line for group0" = "black", "True line for group1" = "brown")
      ggplot(data_current) + geom_point(aes(x, y, color = "Your points for group0")) + 
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(group1 == TRUE & group0 == FALSE){
      colors <- c("Your points for group1" = "red", "True line for group0" = "black", "True line for group1" = "brown")
      ggplot(data_current) + geom_point(aes(x, y, color = "Your points for group1")) + 
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(group0 == FALSE & group1 == FALSE){
      
      colors <- c("True line for group0" = "black", "True line for group1" = "brown")
      ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }
  }else if(submit0 == TRUE & submit1 == FALSE){
    
    colors <- c("Your points for group0" = "blue", "Sample points for group1" = "red", "Your line for group0" = "cyan", "Your line for group1" = "orange", "True line for group0" = "black", "True line for group1" = "brown")
    ggplot() + geom_point(data = data_0, aes(x, y, color = "Your points for group0")) + 
      geom_point(data = data_1, aes(x, y, color = "Sample points for group1")) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
      geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    
  }else if(submit0 == FALSE & submit1 == TRUE){
    
    colors <- c("Sample points for group0" = "blue", "Your points for group1" = "red", "Your line for group0" = "cyan", "Your line for group1" = "orange", "True line for group0" = "black", "True line for group1" = "brown")
    ggplot() + geom_point(data = data_0, aes(x, y, color = "Sample points for group0")) + 
      geom_point(data = data_1, aes(x, y, color = "Your points for group1")) + 
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
      geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
      labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    
  }else{
    if(flag == 0){
      
      colors <- c("Your points for group0" = "blue", "Sample points for group1" = "red", "Your line for group0" = "cyan", "Your line for group1" = "orange", "True line for group0" = "black", "True line for group1" = "brown")
      ggplot() + geom_point(data = data_0, aes(x, y, color = "Your points for group0")) + 
        geom_point(data = data_1, aes(x, y, color = "Sample points for group1")) + 
        geom_line(data = data_0_fit, aes(x, fitted.values, color = "Your line for group0")) +
        geom_line(data = data_1_fit, aes(x, fitted.values, color = "Your line for group1")) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }else if(flag ==1){
      
      colors <- c("Sample points for group0" = "blue", "Your points for group1" = "red", "Your line for group0" = "cyan", "Your line for group1" = "orange", "True line for group0" = "black", "True line for group1" = "brown")
      ggplot() + geom_point(data = data_0, aes(x, y, color = "Sample points for group0")) + 
        geom_point(data = data_1, aes(x, y, color = "Your points for group1")) + 
        geom_line(data = data_0_fit, aes(x, fitted.values, color = "Your line for group0")) +
        geom_line(data = data_1_fit, aes(x, fitted.values, color = "Your line for group1")) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = intercept_assess_draw_cpb + b2_assess_draw_cpb, slope = b1_assess_draw_cpb, color = "True line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
    }
    
  }
  
}