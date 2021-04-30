

plot_module2_binary <- function(binary_group0, binary_group1){
  if(length(binary_group0) != 0 & length(binary_group1) == 0){
    data_group0 <- as.data.frame(matrix(binary_group0,ncol = 2, byrow = T))
    colnames(data_group0) <- c("x","y")
    ggplot(data = data_group0) + geom_point( aes(x, y), color = "blue") +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +  
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +  
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6)
    
  }else if(length(binary_group0) == 0 & length(binary_group1) != 0){
    data_group1 <- as.data.frame(matrix(binary_group1,ncol = 2, byrow = T))
    colnames(data_group1) <- c("x","y")
    ggplot(data = data_group1) +
      geom_point(aes(x = x, y = y), color = "red") +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw()+  
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +  
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6)
    
  }else if(length(binary_group0) != 0 & length(binary_group1) != 0){
    data_group0 <- as.data.frame(matrix(binary_group0,ncol = 2, byrow = T))
    colnames(data_group0) <- c("x","y")
    data_group1 <- as.data.frame(matrix(binary_group1,ncol = 2, byrow = T))
    colnames(data_group1) <- c("x","y")
    ggplot() + geom_point(data = data_group0, aes(x, y), color = "blue") + 
      geom_point(data = data_group1, aes(x = x, y = y), color = "red") +
      scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +  
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +  
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6)
    
  }else{
    data_current <- data.frame()
    ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +  
      annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +  
      annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2) +
      annotate("text",x=0.5,y=10,hjust=0.5,vjust=1.7,label="y = b0 + b1d + e", fontface = "italic", size = 6)
  }
}