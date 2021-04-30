
plot_module4_cmb <- function(line = FALSE, submit = FALSE, data_0, data_1, formula, b0 = NULL, b1 = NULL, b2 = NULL, b3 = NULL){
  
  if(line == TRUE){
    if(submit == TRUE){
      colors <- c( "True line for group0" = "black", "Your line for group0" = "cyan", "True line for group1" = "brown", "Your line for group1" = "orange")
      ggplot() +
        geom_line(data = data_0, aes(x = x, y = y, color = "True line for group0"), inherit.aes = F, size = 1) +
        geom_line(data = data_1, aes(x = x, y = y, color = "True line for group1"), inherit.aes = F, size = 1) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_abline(aes(intercept = b0, slope = b1, color = "Your line for group0"), show.legend = FALSE) +
        geom_abline(aes(intercept = b0 + b2, slope = b1 + b3, color = "Your line for group1"), show.legend = FALSE) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }else{
      colors <- c("True line for group1" = "brown", "True line for group0" = "black")
      ggplot() +
        geom_line(data = data_0, aes(x = x, y = y, color = "True line for group0"), inherit.aes = F, size = 1) +
        geom_line(data = data_1, aes(x = x, y = y, color = "True line for group1"), inherit.aes = F, size = 1) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }
  }else{
    if(submit == TRUE){
      colors <- c( "True line for group0" = "black", "Points for group0 with your sigma" = "cyan", "True line for group1" = "brown", "Points for group1 with your sigma" = "orange", 
                   "Points for group0" = "black", "Points for group1" = "brown")
      ggplot() +
        geom_point(data = data_0, aes(x = x, y = y_sample, color = "Points for group0"), size = 1) +
        geom_point(data = data_1, aes(x = x, y = y_sample, color = "Points for group1"), size = 1) +
        geom_line(data = data_0, aes(x = x, y = y, color = "True line for group0"), inherit.aes = F, size = 1) +
        geom_line(data = data_1, aes(x = x, y = y, color = "True line for group1"), inherit.aes = F, size = 1) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(formula), fontface = "italic", size = 6) +
        geom_point(data = data_0, aes(x = x, y = y_sample_user, color = "Points for group0 with your sigma"), size = 1) +
        geom_point(data = data_1, aes(x = x, y = y_sample_user, color = "Points for group1 with your sigma"), size = 1) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }else{
      colors <- c( "True line for group0" = "black", "True line for group1" = "brown", "Points for group0" = "black", "Points for group1" = "brown")
      ggplot() +
        geom_point(data = data_0, aes(x = x, y = y_sample, color = "Points for group0"), size = 1) +
        geom_point(data = data_1, aes(x = x, y = y_sample, color = "Points for group1"), size = 1) +
        geom_line(data = data_0, aes(x = x, y = y, color = "True line for group0"), inherit.aes = F, size = 1) +
        geom_line(data = data_1, aes(x = x, y = y, color = "True line for group1"), inherit.aes = F, size = 1) +
        expand_limits(x = 0, y = 0) +
        scale_x_continuous(expand = c(0, 0), limits = c(0,10)) +
        scale_y_continuous(expand = c(0, 0), limits = c(0,10)) + theme_bw() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label="y = b0 + b1x + b2d + b3x*d + e", fontface = "italic", size = 6) +
        labs(x = "x", y = "y", color = " ") + scale_color_manual(values = colors) + theme(legend.position="bottom")
    }
  }
}