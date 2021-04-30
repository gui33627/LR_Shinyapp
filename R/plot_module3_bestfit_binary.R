
plot_module3_bestfit_binary <- function(group0 = FALSE, group1 = FALSE, data_group0 = NULL, data_group1 = NULL, data_0, data_1, formula = NULL){
  
  if(group0 == TRUE & group1 == FALSE){
    colors <- c( "Your mean for group0" = "cyan", "Points for group0" = "blue", "Points for group1" = "red")
    jitter <- position_jitter(width = 0.1, height = 0.1, seed = 1)
    ggplot(data = data_group0) + geom_point( aes(0, y, color = "Your mean for group0"), size = 3) +
      geom_point(data = data_0, aes(x = d, y = y_sample, color = "Points for group0"), position = jitter) +
      geom_point(data = data_1, aes(x = d, y = y_sample, color = "Points for group1"), position = jitter) +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
      labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
  }else if(group0 == FALSE & group1 == TRUE){
    
    colors <- c( "Your mean for group1" = "orange", "Points for group0" = "blue", "Points for group1" = "red")
    jitter <- position_jitter(width = 0.1, height = 0.1, seed = 1)
    ggplot(data = data_group1) +
      geom_point(aes(x = 1, y = y, color = "Your mean for group1"), size = 3) +
      geom_point(data = data_0, aes(x = d, y = y_sample, color = "Points for group0"), position = jitter) +
      geom_point(data = data_1, aes(x = d, y = y_sample, color = "Points for group1"), position = jitter) +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
      labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
  }else if(group0 == FALSE & group1 == FALSE){
    colors <- c( "Points for group0" = "blue", "Points for group1" = "red")
    jitter <- position_jitter(width = 0.1, height = 0.1, seed = 1)
    ggplot() +
      geom_point(data = data_0, aes(x = d, y = y_sample, color = "Points for group0"), position = jitter) +
      geom_point(data = data_1, aes(x = d, y = y_sample, color = "Points for group1"), position = jitter) +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6) +
      labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
  }else{
    colors <- c( "True mean for group0" = "black", "Your mean for group0" = "cyan", "True mean for group1" = "brown", "Your mean for group1" = "orange", 
                 "Points for group0" = "blue", "Points for group1" = "red")
    jitter <- position_jitter(width = 0.1, height = 0.1, seed = 1)
    ggplot(data = data_group0) + geom_point( aes(x = 0, y = y, color = "Your mean for group0"), size = 3) +
      geom_point(data = data_group1, aes(x = 1, y = y, color = "Your mean for group1"), size = 3) +
      geom_point(data = data_0, aes(x = d, y = y_sample, color = "Points for group0"), position = jitter) +
      geom_point(data = data_1, aes(x = d, y = y_sample, color = "Points for group1"), position = jitter) +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
      geom_point(aes(x=0, y=mean(data_0$y_sample), color="True mean for group0"), size = 3) +
      geom_point(aes(x=1, y=mean(data_1$y_sample), color="True mean for group1"), size = 3) +
      labs(x = "d", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
  }
}