

plot_module2_continuous_empty <- function(){
  data_current <- data.frame()
  ggplot(data_current) + geom_point() + scale_x_continuous(expand = c(0, 0), limits = c(0,10)) + 
    scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
    annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + e", fontface = "italic", size = 6)
}