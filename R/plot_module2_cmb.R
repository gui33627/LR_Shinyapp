

plot_module2_cmb <- function(continuous_m_binary_group0, continuous_m_binary_group1){
  if(length(continuous_m_binary_group0) != 0 & length(continuous_m_binary_group1) == 0){
    data_group0 <- as.data.frame(matrix(continuous_m_binary_group0,ncol = 2, byrow = T))
    colnames(data_group0) <- c("x","y")
    ggplot(data = data_group0) + geom_point( aes(x, y), color = "blue") +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6)
    
  }else if(length(continuous_m_binary_group0) == 0 & length(continuous_m_binary_group1) != 0){
    data_group1 <- as.data.frame(matrix(continuous_m_binary_group1,ncol = 2, byrow = T))
    colnames(data_group1) <- c("x","y")
    ggplot(data = data_group1) +
      geom_point(aes(x = x, y = y), color = "red") +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6)
  }else if(length(continuous_m_binary_group0) != 0 & length(continuous_m_binary_group1) != 0){
    data_group0 <- as.data.frame(matrix(continuous_m_binary_group0,ncol = 2, byrow = T))
    colnames(data_group0) <- c("x","y")
    data_group1 <- as.data.frame(matrix(continuous_m_binary_group1,ncol = 2, byrow = T))
    colnames(data_group1) <- c("x","y")
    
    ggplot(data = data_group1) + geom_point( aes(x, y), color = "red") + geom_point(data = data_group0, aes(x, y), color = "blue") +
      scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6)
  }else{
    data_current <- data.frame()
    ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
      scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
      annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6)
  }
}