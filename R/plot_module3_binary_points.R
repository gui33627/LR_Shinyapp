

plot_module3_binary_points <- function(formula, intercept_assess_binary_draw, coefficient_assess_binary_draw, samplesize = FALSE, empty = FALSE, data_0 = NULL,data_1 = NULL){
  
  if(empty == TRUE){
    colors <- c( "True mean for group0" = "black", "True mean for group1" = "brown")
    ggplot() + scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_point(aes(x = 0, y = intercept_assess_binary_draw, color = "True mean for group0"), size = 3) + 
      geom_point(aes(x = 1, y = intercept_assess_binary_draw + coefficient_assess_binary_draw, color = "True mean for group1"), size = 3) +
      labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
  }else{
    if(samplesize == TRUE){
      colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange", "Your points for group0" = "blue", "Your points for group1" = "red")
      ggplot() + 
        geom_point(data = data_0, aes(x = x, y = y, color = "Your points for group0"), inherit.aes = F, size = 1) +
        geom_point(data = data_1, aes(x = x, y = y, color = "Your points for group1"), inherit.aes = F, size = 1) +
        geom_point(aes(x = 0, y = mean(data_0$y), color = "Your mean for group0"), inherit.aes = F, size = 3) +
        geom_point(aes(x = 1, y = mean(data_1$y), color = "Your mean for group1"), inherit.aes = F, size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_point(aes(x = 0, y = intercept_assess_binary_draw, color = "True mean for group0"), size = 3) + 
        geom_point(aes(x = 1, y = intercept_assess_binary_draw + coefficient_assess_binary_draw, color = "True mean for group1"), size = 3) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
      
    }else{
      if(nrow(data_0) == 0){
        colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange", "Your points for group0" = "blue", "Your points for group1" = "red")
        ggplot() + 
          geom_point(data = data_1, aes(x = x, y = y, color = "Your points for group1"), inherit.aes = F, size = 1) +
          scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
          annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
          annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
          annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
          geom_point(aes(x = 0, y = intercept_assess_binary_draw, color = "True mean for group0"), size = 3) + 
          geom_point(aes(x = 1, y = intercept_assess_binary_draw + coefficient_assess_binary_draw, color = "True mean for group1"), size = 3) +
          labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
        
      }else if(nrow(data_1) == 0){
        colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange", "Your points for group0" = "blue", "Your points for group1" = "red")
        ggplot() + 
          geom_point(data = data_0, aes(x = x, y = y, color = "Your points for group0"), inherit.aes = F, size = 1) +
          scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
          annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
          annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
          annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
          geom_point(aes(x = 0, y = intercept_assess_binary_draw, color = "True mean for group0"), size = 3) + 
          geom_point(aes(x = 1, y = intercept_assess_binary_draw + coefficient_assess_binary_draw, color = "True mean for group1"), size = 3) +
          labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
        
      }else{
        colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange", "Your points for group0" = "blue", "Your points for group1" = "red")
        ggplot() + 
          geom_point(data = data_0, aes(x = x, y = y, color = "Your points for group0"), inherit.aes = F, size = 1) +
          geom_point(data = data_1, aes(x = x, y = y, color = "Your points for group1"), inherit.aes = F, size = 1) +
          scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
          scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
          annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
          annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
          annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
          geom_point(aes(x = 0, y = intercept_assess_binary_draw, color = "True mean for group0"), size = 3) + 
          geom_point(aes(x = 1, y = intercept_assess_binary_draw + coefficient_assess_binary_draw, color = "True mean for group1"), size = 3) +
          labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
        
      }
    }
    
    
  }
  
}