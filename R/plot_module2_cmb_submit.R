

plot_module2_cmb_submit <- function(data_1, data_0, data_full, formula, sigma){
  
  colors <- c("Your points for group0" = "blue", "Regression line for group0" = "blue", "Regression line for group1" = "red","Your points for group1" = "red", "error" = "black" )
  ggplot(data = data_1) + geom_point( aes(x, y, color = "Your points for group1")) +
    geom_line(aes(x = x, y = fitted.values, color = "Regression line for group1"), inherit.aes = F, size = 1) + 
    geom_point(data = data_0, aes(x, y, color = "Your points for group0")) +
    geom_line(data = data_0, aes(x = x, y = fitted.values, color = "Regression line for group0"), inherit.aes = F, size = 1) + 
    scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
    scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
    geom_segment(data =data_full, aes(x=x, xend=x, y=y, yend=fitted.values, color="error")) +
    geom_text(data =data_full, aes(x=x-0.15, y=(y+fitted.values)/2,  label = error)) +
    annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
    annotate("text",x=8, y = 0.5, label = TeX(sigma) ) +
    labs(x = "x", y = "y", color = " ") + scale_color_manual( values = colors) + theme(legend.position="bottom") 
  
}