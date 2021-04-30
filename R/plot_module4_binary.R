
plot_module4_binary <- function(mean = FALSE, submit = FALSE, data_0, data_1, b0 = NULL, b1 = NULL, formula = NULL){
  
  if(mean == TRUE){
    if(submit == TRUE){
      colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange")
      ggplot() + 
        geom_point(aes(x = 0, y = b0, color = "Your mean for group0"), inherit.aes = F, size = 3) +
        geom_point(aes(x = 1, y = b0 + b1, color = "Your mean for group1"), inherit.aes = F, size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_point(aes(x = 0, y = mean(data_0$y), color = "True mean for group0"), size = 3) + 
        geom_point(aes(x = 1, y = mean(data_1$y), color = "True mean for group1"), size = 3) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
      
    }else{
      colors <- c( "True mean for group0" = "black", "True mean for group1" = "brown")
      ggplot() + 
        geom_point(aes(x = 0, y = mean(data_0$y), color = "True mean for group0"), inherit.aes = F, size = 3) +
        geom_point(aes(x = 1, y = mean(data_1$y), color = "True mean for group1"), inherit.aes = F, size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
    }
  }else{
    if(submit == TRUE){
      colors <- c( "True mean for group0" = "blue", "Points for group0 with your sigma" = "cyan", "True mean for group1" = "red", "Points for group1 with your sigma" = "orange", "Points for group0" = "black", "Points for group1" = "brown")
      jitter <- position_jitter(width = 0.1, height = 0.1)
      ggplot() + 
        geom_point(data = data_0, aes(x = d, y = y_sample, color = "Points for group0"), inherit.aes = F, size = 1, position = jitter) +
        geom_point(data = data_1, aes(x = d, y = y_sample, color = "Points for group1"), inherit.aes = F, size = 1, position = jitter) +
        geom_point(data = data_0, aes(x = d, y = y_sample_user, color = "Points for group0 with your sigma"), inherit.aes = F, size = 1, position = jitter) +
        geom_point(data = data_1, aes(x = d, y = y_sample_user, color = "Points for group1 with your sigma"), inherit.aes = F, size = 1, position = jitter) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5, y=10, hjust=0.5, vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_point(aes(x = 0, y = mean(data_0$y), color = "True mean for group0"), size = 3) + 
        geom_point(aes(x = 1, y = mean(data_1$y), color = "True mean for group1"), size = 3) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
      
    }else{
      colors <- c( "True mean for group0" = "blue", "True mean for group1" = "red", "Points for group0" = "black", "Points for group1" = "brown")
      jitter <- position_jitter(width = 0.1, height = 0.1)
      ggplot() + 
        geom_point(data = data_0, aes(x = d, y = y_sample, color = "Points for group0"), inherit.aes = F, size = 1, position = jitter) +
        geom_point(data = data_1, aes(x = d, y = y_sample, color = "Points for group1"), inherit.aes = F, size = 1, position = jitter) +
        geom_point(aes(x = 0, y = mean(data_0$y), color = "True mean for group0"), inherit.aes = F, size = 3) +
        geom_point(aes(x = 1, y = mean(data_1$y), color = "True mean for group1"), inherit.aes = F, size = 3) +
        scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
        annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
        labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom")
    }
    
  }
}