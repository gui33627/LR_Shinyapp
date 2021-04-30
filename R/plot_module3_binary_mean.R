

plot_module3_binary_mean <- function(empty = FALSE, data_group0 = NULL, data_group1 = NULL, formula = NULL, intercept_assess_binary_draw = NULL, coefficient_assess_binary_draw = NULL){
  if(empty == TRUE){
    
    ggplot(data.frame()) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6)
  }else{
    if(nrow(data_group0) != 0 & is.null(data_group1)){
      
      colors <- c( "Your mean for group0" = "cyan")
      ggplot(data = data_group0) + geom_point( aes(0, y, color = "Your mean for group0"), size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
      
    }else if(is.null(data_group0) & nrow(data_group1) != 0){
      
      colors <- c( "Your mean for group1" = "orange")
      ggplot(data = data_group1) +
        geom_point(aes(x = 1, y = y, color = "Your mean for group1"), size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw()+
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
      
    }else{
      colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange")
      ggplot(data = data_group0) + geom_point( aes(0, y, color = "Your mean for group0"), size = 3) +
        geom_point(data = data_group1, aes(x = 1, y = y, color = "Your mean for group1"), size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label= as.character(formula), fontface = "italic", size = 6) +
        geom_point(aes(x=0, y=intercept_assess_binary_draw, color="True mean for group0"), size = 3) +
        geom_point(aes(x=1, y=intercept_assess_binary_draw + coefficient_assess_binary_draw, color="True mean for group1"), size = 3) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }
  }
  
}