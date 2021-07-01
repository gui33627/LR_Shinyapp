

SpecifyCoef <- function(input, output, session){
  
  eps_noise <- reactive({
    rnorm(n = input$sample_size, sd = input$epsilon_error)
  })
  
  # generate real relationship 
  real_functional_relationship <- reactive({
    if (input$model_type == "continuous"){
      data <- data.frame(x = seq(from = input$x_range[1], to = input$x_range[2], length.out = 500))
      data$y_values_actual <- with(data, x*input$select_b1 + input$select_b0)
    } else if(input$model_type == "binary"){
      data <- data.frame(D=c(rep(0,250), rep(1,250)), group = rep(c("0 (original)", "1 (original)"), each = 250))
      data$y_values_actual <- with(data, D*input$select_b1D + input$select_b0)
    }else {
      data <- data.frame(x = rep(seq(from = input$x_range[1], to = input$x_range[2], length.out = 500), 2), 
                         D=c(rep(0,500), rep(1,500)), group = rep(c("0 (original)", "1 (original)"), each = 500))
      if (input$model_type == "continuous+binary"){
        data$y_values_actual <-  with(data, x*input$select_b1p + D*input$select_b2p + input$select_b0)
      } else{
        data$y_values_actual <-  with(data, x*input$select_b1m + D*input$select_b2m + x*D*input$select_b3m + input$select_b0)
      }
    }
    data
  })
  
  # generate sample
  sample_tibble <- reactive({
    data <- data.frame(x = runif(n = input$sample_size, min = input$x_range[1], max = input$x_range[2]))
    if (input$model_type == "continuous"){
      data$y_randomized <-  with(data, x*input$select_b1 + input$select_b0 + eps_noise())
    } else{
      data$D <- sample(c(0,1), input$sample_size, replace = T)
      if(input$model_type == "binary"){
        data$y_randomized <-  with(data, D*input$select_b1D + input$select_b0 + eps_noise())
      } else if (input$model_type == "continuous+binary"){
        data$y_randomized <-  with(data, x*input$select_b1p + D*input$select_b2p + input$select_b0 + eps_noise())
      } else {
        data$y_randomized <-  with(data, x*input$select_b1m + D*input$select_b2m + x*D*input$select_b3m + input$select_b0 + eps_noise())
      }
    } 
    data
  })
  
  linear_model_predictions <- reactive({
    if (input$model_type == "continuous"){
      linear_model <- lm(y_randomized ~ x, data = sample_tibble())
      if(summary(linear_model)$coefficients[2] >= 0){
        output$interpretation <- renderText(interpretation_continuous(input$select_b1, positive = TRUE))
      }else if(summary(linear_model)$coefficients[2] < 0){
        output$interpretation <- renderText(interpretation_continuous(input$select_b1, positive = FALSE))
      }
    } else if (input$model_type == "binary"){
      linear_model <- lm(y_randomized ~ D, data = sample_tibble())
      if(summary(linear_model)$coefficients[2] >= 0){
        output$interpretation <- renderText(interpretation_binary(input$select_b1D, positive = TRUE))
      }else if(summary(linear_model)$coefficients[2] < 0){
        output$interpretation <- renderText(interpretation_binary(input$select_b1D, positive = FALSE))
      }
    } else if (input$model_type == "continuous+binary"){
      linear_model <- lm(y_randomized ~ x + D, data = sample_tibble())
      if(summary(linear_model)$coefficients[2] >= 0){
        output$interpretation <- renderText(interpretation_cpb(input$select_b1p, positive = TRUE))
        output$another_explain0 <- renderText("It's often easier to interpret these models by separately considering the model when d=0 and when d=1.")
        output$another_explain1 <- renderText("Consider the model when d=0")
        output$another_explain3 <- renderText("Consider the model when d=1")
        output$another_explain5 <- renderText("Then each of these can be interpreted as in the model y = b0 + b1x + e.")  
        
      }else if(summary(linear_model)$coefficients[2] < 0){
        output$interpretation <- renderText(interpretation_cpb(input$select_b1p, positive = FALSE))
        output$another_explain0 <- renderText("It's often easier to interpret these models by separately considering the model when d=0 and when d=1.")
        output$another_explain1 <- renderText("Consider the model when d=0")
        output$another_explain3 <- renderText("Consider the model when d=1")
        output$another_explain5 <- renderText("Then each of these can be interpreted as in the model y = b0 + b1x + e.") 
      }
      
    } else {
      linear_model <- lm(y_randomized ~ x*D, data = sample_tibble())
      if(summary(linear_model)$coefficients[2] >= 0 & (summary(linear_model)$coefficients[2] + summary(linear_model)$coefficients[4] ) >= 0){
        output$interpretation <- renderText(interpretation_cmb(input$select_b1m, input$select_b3m, positive1 = TRUE, positive2 = TRUE))
        output$another_explain0 <- renderText("It's often easier to interpret these models by separately considering the model when d=0 and when d=1.")
        output$another_explain1 <- renderText("Consider the model when d=0")
        output$another_explain3 <- renderText("Consider the model when d=1")
        output$another_explain5 <- renderText("Then each of these can be interpreted as in the model y = b0 + b1x + e.") 
      }else if(summary(linear_model)$coefficients[2] < 0 & (summary(linear_model)$coefficients[2] + summary(linear_model)$coefficients[4] ) < 0){
        output$interpretation <- renderText(interpretation_cmb(input$select_b1m, input$select_b3m, positive1 = FALSE, positive2 = FALSE))
        output$another_explain0 <- renderText("It's often easier to interpret these models by separately considering the model when d=0 and when d=1.")
        output$another_explain1 <- renderText("Consider the model when d=0")
        output$another_explain3 <- renderText("Consider the model when d=1")
        output$another_explain5 <- renderText("Then each of these can be interpreted as in the model y = b0 + b1x + e.") 
      }else if(summary(linear_model)$coefficients[2] >= 0 & (summary(linear_model)$coefficients[2] + summary(linear_model)$coefficients[4] ) < 0){
        output$interpretation <- renderText(interpretation_cmb(input$select_b1m, input$select_b3m, positive1 = TRUE, positive2 = FALSE))
        output$another_explain0 <- renderText("It's often easier to interpret these models by separately considering the model when d=0 and when d=1.")
        output$another_explain1 <- renderText("Consider the model when d=0")
        output$another_explain3 <- renderText("Consider the model when d=1")
        output$another_explain5 <- renderText("Then each of these can be interpreted as in the model y = b0 + b1x + e.") 
      }else{
        output$interpretation <- renderText(interpretation_cmb(input$select_b1m, input$select_b3m, positive1 = FALSE, positive2 = TRUE))
        output$another_explain0 <- renderText("It's often easier to interpret these models by separately considering the model when d=0 and when d=1.")
        output$another_explain1 <- renderText("Consider the model when d=0")
        output$another_explain3 <- renderText("Consider the model when d=1")
        output$another_explain5 <- renderText("Then each of these can be interpreted as in the model y = b0 + b1x + e.") 
      }
    }
    confidence_interval <- as.data.frame(predict(linear_model, interval = "confidence", newdata = real_functional_relationship()))
    colnames(confidence_interval) <- unlist(sapply(colnames(confidence_interval), function(x) paste0("ci_", x)))
    prediction_interval <- as.data.frame(predict(linear_model, interval = "prediction", newdata = real_functional_relationship()))
    colnames(prediction_interval) <- unlist(sapply(colnames(prediction_interval), function(x) paste0("pi_", x)))
    prepped_tibble <- cbind(real_functional_relationship(), confidence_interval, prediction_interval)
    prepped_tibble
  })
  
  output$result_plot <- renderPlot({
    if(input$model_type == "continuous"){
      req(input$select_b0)
      req(input$select_b1)
      data <-  paste0("y = ", input$select_b0, " + ", input$select_b1, "x + e, ", "e~N(0, ",input$epsilon_error,"^2)")
      output$formula <- renderText(data)
      final_plot <- ggplot() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(data), fontface = "italic", size = 6)
    }else if(input$model_type == "binary"){
      req(input$select_b0)
      req(input$select_b1D)
      data <-  paste0("y = ", input$select_b0, " + ", input$select_b1D, "D + e, ", "e~N(0, ",input$epsilon_error,"^2)")
      output$formula <- renderText(data)
      final_plot <- ggplot() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(data), fontface = "italic", size = 6)
    }else if(input$model_type == "continuous+binary"){
      req(input$select_b0)
      req(input$select_b1p)
      req(input$select_b2p)
      data <-  paste0("y = ", input$select_b0, " + ", input$select_b1p, "x + ",input$select_b2p, "D + e, ", "e~N(0, ",input$epsilon_error,"^2)")
      output$formula <- renderText(data)
      output$another_explain2 <- renderText(paste0("y = ", input$select_b0, " + ", input$select_b1p, "x + e"))
      output$another_explain4 <- renderText(paste0("y = (", input$select_b0, " + ", input$select_b2p, ") + ", input$select_b1p, "x + e"))
      final_plot <- ggplot() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.15,vjust=1.7,label=as.character(data), fontface = "italic", size = 6)
    }else{
      req(input$select_b0)
      req(input$select_b1m)
      req(input$select_b2m)
      req(input$select_b3m)
      data <-  paste0("y = ", input$select_b0, " + ", input$select_b1m, "x + ",input$select_b2m, "D + ", input$select_b3m, "x*D + e, e~N(0, ",input$epsilon_error,"^2)")
      output$formula <- renderText(data)
      output$another_explain2 <- renderText(paste0("y = ", input$select_b0, " + ", input$select_b1m, "x + e"))
      output$another_explain4 <- renderText(paste0("y = (", input$select_b0, " + ",input$select_b2m,") + (", input$select_b1m, " + ",input$select_b3m,")x + e"))
      final_plot <- ggplot() +
        annotate("text",x=-Inf,y=Inf,hjust=-0.1,vjust=1.7,label=as.character(data), fontface = "italic", size = 6)
    }
    
    if (input$include_real_function){
      if(input$model_type == "continuous"){
        final_plot <- final_plot + geom_line(data = real_functional_relationship(), aes(x = x, y = y_values_actual), color = "black", inherit.aes = F, size = 1.5)
      }else if(input$model_type == "binary"){
        colors <- c("0" = "blue", "1" = "red", "0 (original)" = "black", "1 (original)" = "brown")
        final_plot <- final_plot +
          geom_point(data = real_functional_relationship(), aes(x = D, y = y_values_actual, group=as.factor(group), color = as.factor( group)), inherit.aes = F, size = 3) + scale_color_manual(values = colors) +
          labs(color= "Binary Variable")
      }else{
        colors <- c("0" = "blue", "1" = "red", "0 (original)" = "black", "1 (original)" = "brown")
        final_plot <- final_plot + 
          geom_line(data = real_functional_relationship(), aes(x = x, y = y_values_actual, group=as.factor( group), color = as.factor( group)), inherit.aes = F, size = 1.5) + scale_color_manual(values = colors) + 
          labs(color= "Binary Variable")
      }
    }
    
    if (input$include_geom_point){
      if(input$model_type == "binary"){
        jitter <- position_jitter(width = 0.1, height = 0.1)
        final_plot <- final_plot + 
          geom_point(data = sample_tibble(), aes(x = D, y = y_randomized), alpha = 0.5, inherit.aes = F, position = jitter)
      }else{
        final_plot <- final_plot + 
          geom_point(data = sample_tibble(), aes(x = x, y = y_randomized), alpha = 0.5, inherit.aes = F)
      }
    }
    
    if (input$include_ci){
      if(input$model_type == "continuous"){
        final_plot <- final_plot + 
          geom_line(data = linear_model_predictions(), aes(x = x, y = ci_lwr), inherit.aes = F, color = "cyan", linetype = 2, size = 1) + 
          geom_line(data = linear_model_predictions(), aes(x = x, y = ci_upr), inherit.aes = F, color = "cyan", linetype = 2, size = 1)
      } else if(input$model_type == "binary"){
        data_0 <- subset(linear_model_predictions(), D==0)
        data_1 <- subset(linear_model_predictions(), D==1)
        final_plot <- final_plot + 
          geom_point(data = data_0, aes(x = D, y = ci_lwr), inherit.aes = F, color = "cyan", size = 1) + 
          geom_point(data = data_0, aes(x = D, y = ci_upr), inherit.aes = F, color = "cyan", size = 1) + 
          geom_point(data = data_1, aes(x = D, y = ci_lwr), inherit.aes = F, color = "purple", size = 1) + 
          geom_point(data = data_1, aes(x = D, y = ci_upr), inherit.aes = F, color = "purple", size = 1)
      }else{
        data_0 <- subset(linear_model_predictions(), D==0)
        data_1 <- subset(linear_model_predictions(), D==1)
        final_plot <- final_plot + 
          geom_line(data = data_0, aes(x = x, y = ci_lwr), inherit.aes = F, color = "cyan", linetype = 2, size = 1) + 
          geom_line(data = data_0, aes(x = x, y = ci_upr), inherit.aes = F, color = "cyan", linetype = 2, size = 1) + 
          geom_line(data = data_1, aes(x = x, y = ci_lwr), inherit.aes = F, color = "purple", linetype = 2, size = 1) + 
          geom_line(data = data_1, aes(x = x, y = ci_upr), inherit.aes = F, color = "purple", linetype = 2, size = 1)
      }
    }
    
    if (input$include_pi){
      if(input$model_type == "continuous"){
        final_plot <- final_plot + 
          geom_line(data = linear_model_predictions(), aes(x = x, y = pi_lwr), inherit.aes = F, color = "green", linetype = 2, size = 1)+ 
          geom_line(data = linear_model_predictions(), aes(x = x, y = pi_upr), inherit.aes = F, color = "green", linetype = 2, size = 1)
      } else if((input$model_type == "binary")){
        data_0 <- subset(linear_model_predictions(), D==0)
        data_1 <- subset(linear_model_predictions(), D==1)
        final_plot <- final_plot + 
          geom_point(data = data_0, aes(x = D, y = pi_lwr), inherit.aes = F, color = "green", size = 1) + 
          geom_point(data = data_0, aes(x = D, y = pi_upr), inherit.aes = F, color = "green", size = 1) + 
          geom_point(data = data_1, aes(x = D, y = pi_lwr), inherit.aes = F, color = "orange", size = 1) + 
          geom_point(data = data_1, aes(x = D, y = pi_upr), inherit.aes = F, color = "orange", size = 1)
      }else{
        data_0 <- subset(linear_model_predictions(), D==0)
        data_1 <- subset(linear_model_predictions(), D==1)
        final_plot <- final_plot + 
          geom_line(data = data_0, aes(x = x, y = pi_lwr), inherit.aes = F, color = "green", linetype = 2, size = 1) + 
          geom_line(data = data_0, aes(x = x, y = pi_upr), inherit.aes = F, color = "green", linetype = 2, size = 1) + 
          geom_line(data = data_1, aes(x = x, y = pi_lwr), inherit.aes = F, color = "orange", linetype = 2, size = 1) + 
          geom_line(data = data_1, aes(x = x, y = pi_upr), inherit.aes = F, color = "orange", linetype = 2, size = 1)
      }
    }
    
    if (input$include_fit){
      if(input$model_type == "continuous"){
        final_plot <- final_plot +
          geom_line(data = linear_model_predictions(), aes(x = x, y = ci_fit), inherit.aes = F, color = "blue", size = 1)
      }else if(input$model_type == "binary"){
        colors <- c("0" = "blue", "1" = "red", "0 (original)" = "black", "1 (original)" = "brown")
        final_plot <- final_plot +
          geom_point(data = linear_model_predictions(), aes(x = D, y = ci_fit, color = as.factor(D)), inherit.aes = F, size = 3)+ scale_color_manual(values = colors) + 
          labs(color= "Binary Variable")
      }else{
        data_0 <- subset(linear_model_predictions(), D==0)
        data_1 <- subset(linear_model_predictions(), D==1)
        colors <- c("0" = "blue", "1" = "red", "0 (original)" = "black", "1 (original)" = "brown")
        final_plot <- final_plot +
          geom_line(data = data_0, aes(x = x, y = ci_fit, color = "0"), inherit.aes = F,  size = 1) +
          geom_line(data = data_1, aes(x = x, y = ci_fit, color = "1",), inherit.aes = F,  size = 1) + scale_color_manual(values = colors) + 
          labs(color= "Binary Variable")
      }
    }
    
    if(input$model_type == "binary"){
      final_plot + 
        theme_bw() + 
        xlab("D") + 
        ylab("y")
    }else{
      final_plot + 
        theme_bw() + 
        xlab("x") + 
        ylab("y")
    }
  })
}