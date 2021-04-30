

plot_module2_continuous_line <- function(data_current, formula, sample) {
  colors <- c("Your regression line" = "brown", "Sample points" = "black")
  p <- ggplot(data = data_current, aes(x, y)) + geom_point() + 
    geom_line(aes(x = x, y = y, color = "Your regression line"), inherit.aes = F,  size = 1) +
    expand_limits(x = 0, y = 0) +
    scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() + 
    geom_point(data = sample, aes(x = x, y = y, color = "Sample points"), alpha=0.5) + 
    annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
    labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
  
  return(p)
}