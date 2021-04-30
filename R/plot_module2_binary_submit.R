

plot_module2_binary_submit <- function(data_group0, data_group1, data_full, formula, sigma){
  colors <- c("Your points for group0" = "blue", "Your points for group1" = "red", "Mean for your group0" = "cyan", "Mean for your group1" = "orange", "error" = "black" )
  p <- ggplot() + geom_point(data = data_group0, aes(x, y, color = "Your points for group0")) + 
    geom_point(data = data_group1, aes(x = x, y = y, color = "Your points for group1")) +
    scale_x_continuous(expand = c(0, 0), limits = c(-0.1,1.1)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +  
    annotate("rect", xmin = -0.1, xmax = 0.1, ymin = 0, ymax = 10, alpha = .2) +  
    annotate("rect", xmin = 0.9, xmax = 1.1, ymin = 0, ymax = 10, alpha = .2)  + 
    geom_segment(data = data_full, aes(x=x, xend=x, y=y, yend=fitted.values, color="error")) +
    geom_text(data = data_full, aes(x=x-0.02, y=(y+fitted.values)/2,  label = error)) +
    annotate("text",x=-Inf,y=Inf,hjust=-1.2,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
    annotate("text",x=0.5, y = 0.2, vjust=-1.2,label = TeX(sigma)) +
    geom_point(aes(0, mean(data_group0$y), color = "Mean for your group0"), size = 3) +
    geom_point(aes(1, mean(data_group1$y), color = "Mean for your group1"), size = 3)  +
    labs(x = "d", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
  return(p)
}