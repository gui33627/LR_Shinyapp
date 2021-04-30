
SpecifyPlot <- function(input, output, session){
  
  ############ continuous ###############
  
  newdata1 <- reactiveValues(data = c())
  newdata <- reactiveValues(data = c())
  eps_noise_draw <- reactive({
    rnorm(n = input$sample_size_draw, sd = input$epsilon_error_draw)
  })
  
  which_graph <- reactive({
    if (input$draw_type == "draw_line") {
      if(length(newdata1$data) != 0){
        data_current <- clean_to_dataframe(newdata1$data)
        data_current <- clean_specified_size(data_current, 2)
        slope <- (data_current$y[2] - data_current$y[1])/(data_current$x[2] - data_current$x[1])
        intercept <- data_current$y[1] - slope*data_current$x[1]
        table <- data.frame(b0= round(intercept,2), b1=round(slope,2), sigma = input$epsilon_error_draw )
        output$draw_equation <- renderTable(table)
        sample <- data.frame(x=seq(from = 0, to = 10, length.out = input$sample_size_draw))
        sample$y <- sample$x*slope + intercept + eps_noise_draw()
        if(nrow(data_current) >=2){
          formula <- paste0("y = ", round(intercept,2), " + ", round(slope,2), "x + e, ", "e ~ N(0, ", input$epsilon_error_draw,"^2)" )
        }else{
          formula <- "y = b0 + b1x + e"
        }
        plot_module2_continuous_line(data_current, formula, sample)
      }else{
        plot_module2_continuous_empty()
      }
    } else {
      if(length(newdata$data) != 0){
        data_current <- clean_to_dataframe(newdata$data)
        model <- lm(y~x, data = data_current)
        data_current$fitted.values <- model$fitted.values
        output$lm_results <- renderTable(summary(model)$coefficients, rownames = TRUE,colnames = TRUE)
        output$n <- renderText(paste0("Sample size: ", nrow(data_current)))
        output$sigma <- renderText(paste0("Residual standard deviation: ",round(summary(model)$sigma,2)))
        if(nrow(data_current) >= 3 ){
          formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + e, ", "e ~ N(0, ", round(summary(model)$sigma,2),"^2)" )
          data_current <- data_current %>% mutate(error = paste0("e", row_number()), error_sq = paste0("e", row_number(),"^2")) 
          sigma <- create_sigma_formula(data_current$error_sq[1],data_current$error_sq[nrow(data_current)],nrow(data_current), 2, round(summary(model)$sigma,2) )
          confidence_interval <- as.data.frame(predict(model, interval = "confidence", newdata = data_current))
          colnames(confidence_interval) <- unlist(sapply(colnames(confidence_interval), function(x) paste0("ci_", x)))
          data_current <- cbind(data_current, confidence_interval)
          plot_module2_continuous_pointsn(data_current, formula, sigma)
        }else{
          formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + e")
          plot_module2_continuous_pointsn(data_current, formula)
        }
      } else{
        plot_module2_continuous_empty()
      }
    }
  })
  
  output$plot_specified <- renderPlot({
    which_graph()
  })
  
  observeEvent(input$plot_specified_click, {
    pt <- input$plot_specified_click
    if(input$draw_type == "draw_line"){
      newdata1$data <- c(newdata1$data, pt$x, pt$y)
    }else{
      newdata$data <- c(newdata$data, pt$x, pt$y)
    }
  })
  observeEvent(input$reset, {
    newdata$data <- c()
    newdata1$data <- c()
    output$draw_equation <- renderTable(data.frame())
    output$lm_results <- renderTable(data.frame())
    output$sigma <- renderText("")
    output$n <- renderText("")
  })
  
  
  ########## binary ############
  
  binary_group0 <- reactiveValues(data = c())
  binary_group1 <- reactiveValues(data = c())
  
  binary_done <- reactiveValues(data = FALSE)
  
  which_graph_binary <- reactive({
    group0 <- binary_group0$data
    group1 <- binary_group1$data
    plot_module2_binary(group0, group1)
  })
  
  which_graph_binary_submit <- reactive({
    data_group0 <- clean_to_dataframe(binary_group0$data) 
    data_group1 <- clean_to_dataframe(binary_group1$data)
    data_group0$d <- 0
    data_group1$d <- 1
    data_full <- rbind(data_group0, data_group1)
    model <- lm(y ~ as.factor(d), data = data_full)
    data_full$fitted.values <- model$fitted.values
    data_full <- data_full %>% mutate(error = paste0("e", row_number()), error_sq = paste0("e", row_number(),"^2")) 
    
    if(nrow(data_full) <=2){
      sigma <- paste0("The degree of freedom is ",nrow(data_full) , " - 2, smaller or equal to 0, so sigma cannot be calculated.")
      formula <- paste0("y = ", round(mean(data_group0$y),2), " + ", round(mean(data_group1$y) - mean(data_group0$y),2), "d + e" )
    }else{
      sigma <- create_sigma_formula(data_full$error_sq[1],data_full$error_sq[nrow(data_full)], nrow(data_full), 2, round(summary(model)$sigma, 2))
      formula <- paste0("y = ", round(mean(data_group0$y),2), " + ", round(mean(data_group1$y) - mean(data_group0$y),2), "d + e, ", "e ~ N(0, ", round(summary(model)$sigma, 2),"^2)" )
    }
    plot_module2_binary_submit(data_group0, data_group1, data_full, formula, sigma)
  })
  
  output$plot_specified_binary <- renderPlot({
    if(binary_done$data == TRUE){
      which_graph_binary_submit()
    }else{
      which_graph_binary()
    }
  })
  
  observeEvent(input$plot_specified_click_binary, {
    pt <- input$plot_specified_click_binary
    if(pt$x <= 0.1){
      binary_group0$data <- c(binary_group0$data, pt$x, pt$y)
    }else if(pt$x >= 0.9){
      binary_group1$data <- c(binary_group1$data, pt$x, pt$y)
    }
  })
  
  observeEvent(input$submit_binary, {
    if(length(binary_group0$data) == 0 | length(binary_group1$data) == 0){
      output$binary_mean <- renderText("Please create points for the other group!")
    }else{
      data_group0 <-  clean_to_dataframe(binary_group0$data)
      data_group1 <- clean_to_dataframe(binary_group1$data)
      output$binary_mean <- renderText(paste0("The mean of group 0 is ", round(mean(data_group0$y),2), "; the mean of group 1 is ", round(mean(data_group1$y),2)))
      output$binary_model <- renderText(paste0("The linear model with one binary predictor is y = b0 + b1d + e. 
                                             The d is the binary variable indicating an observation belonging to group 0 or 1. Based on the data you submit, the parameters are:"))
      y <- c(data_group0$y, data_group1$y)
      d <- c(rep(0,length(data_group0$y)), rep(1,length(data_group1$y)))
      model <- lm(y ~ as.factor(d))
      table <- data.frame(b0= round(mean(data_group0$y),2), b1= round(mean(data_group1$y) - mean(data_group0$y),2), sigma= round(summary(model)$sigma, 2))
      output$binary_equation <- renderTable(table)
      binary_done$data <- TRUE
    }
  })
  
  observeEvent(input$reset_binary_model, {
    binary_group0$data <- c()
    binary_group1$data <- c()
    output$binary_mean <- renderText("")
    output$binary_model <- renderText("")
    output$binary_equation <- renderTable(data.frame())
    binary_done$data <- FALSE
  })
  
  ########## continuous+binary ############
  
  continuous_p_binary_group0 <- reactiveValues(data = c())
  continuous_p_binary_group1 <- reactiveValues(data = c())
  submit_one_group <- reactiveValues(data = FALSE)
  
  which_graph_continuous_p_binary <- reactive({
    group0 <- continuous_p_binary_group0$data
    group1 <- continuous_p_binary_group1$data
    plot_module2_cpb(group0, group1)
  })
  
  which_graph_continuous_p_binary_submit <- reactive({
    if(length(continuous_p_binary_group0$data) != 0 & length(continuous_p_binary_group1$data) == 0){
      data_group0 <- clean_to_dataframe(continuous_p_binary_group0$data)
      model <- lm(y~x, data = data_group0)
      data_group0$fitted.values <- model$fitted.values
      diff <- input$diff_gr1_gr0
      data_group1 <- sample_othe_group(data_group0, module2 = TRUE, diff = diff)
      data_full <- rbind(data_group0, data_group1)
      data_full <- data_full %>% mutate(error = paste0("e", row_number()), error_sq = paste0("e", row_number(),"^2"), res_sq = (y - fitted.values)^2) 
      data_sigma <-round(sqrt(sum(data_full$res_sq)/(nrow(data_full)-3)),2)
      
      output$result_continuous_binary <- renderText(paste0("The linear model with one continuous and one binary predictor is y = b0 + b1x + b2d + e.
                                               The d is the binary variable indicating an observation belonging to group 0 or 1. Based on the data you submit, the parameters are:" ))
      table <- data.frame(b0 = round(summary(model)$coefficients[1],2), b1 = round(summary(model)$coefficients[2],2), b2 = input$diff_gr1_gr0, sigma = data_sigma)
      output$continuous_binary_equation <- renderTable(table)
      
      if(nrow(data_full) <=3){
        sigma <- paste0("The degree of freedom is ",nrow(data_full) , " - 3, smaller or equal to 0, so sigma cannot be calculated.")
        formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + ", input$diff_gr1_gr0, "d + ", "e")
      }else{
        sigma <- create_sigma_formula(data_full$error_sq[1],data_full$error_sq[nrow(data_full)], nrow(data_full), 3, data_sigma)
        formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + ", input$diff_gr1_gr0, "d + ", "e, ", "e ~ N(0, ", data_sigma,"^2)")
      }
      plot_module2_cpb_submit(data_group0, data_group1, data_full, formula, sigma, group0 = TRUE)
      
    }else if(length(continuous_p_binary_group0$data) == 0 & length(continuous_p_binary_group1$data) != 0){
      data_group1 <- clean_to_dataframe(continuous_p_binary_group1$data)
      model <- lm(y~x, data = data_group1)
      data_group1$fitted.values <- model$fitted.values
      diff <- input$diff_gr0_gr1
      data_group0 <- sample_othe_group(data_group1, module2 = TRUE, diff = diff)
      data_full <- rbind(data_group0, data_group1)
      data_full <- data_full %>% mutate(error = paste0("e", row_number()), error_sq = paste0("e", row_number(),"^2"), res_sq = (y - fitted.values)^2) 
      data_sigma <-round(sqrt(sum(data_full$res_sq)/(nrow(data_full)-3)),2)
      
      output$result_continuous_binary <- renderText(paste0("The linear model with one continuous and one binary predictor is y = b0 + b1x + b2d + e.
                                               The d is the binary variable indicating an observation belonging to group 0 or 1. Based on the data you submit, the parameters are:" ))
      table <- data.frame(b0 = round(summary(model)$coefficients[1]+input$diff_gr0_gr1,2), b1 = round(summary(model)$coefficients[2],2), b2 = -input$diff_gr0_gr1, sigma = data_sigma)
      output$continuous_binary_equation <- renderTable(table)
      if(nrow(data_full) <=3){
        sigma <- paste0("The degree of freedom is ",nrow(data_full) , " - 3, smaller or equal to 0, so sigma cannot be calculated.")
        formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + ", input$diff_gr0_gr1, "d + ", "e")
      }else{
        sigma <- create_sigma_formula(data_full$error_sq[1], data_full$error_sq[nrow(data_full)],nrow(data_full), 3, data_sigma)
        formula <- paste0("y = ", round(summary(model)$coefficients[1]+input$diff_gr0_gr1,2), " + ", round(summary(model)$coefficients[2],2), "x + ", -input$diff_gr0_gr1, "d + ", "e, ", "e ~ N(0, ", data_sigma,"^2)")
      }
      plot_module2_cpb_submit(data_group0, data_group1, data_full, formula, sigma, group0 = FALSE)
    }
  })
  
  output$plot_specified_continuous_p_binary <- renderPlot({
    if(submit_one_group$data == TRUE){
      which_graph_continuous_p_binary_submit()
    }else{
      which_graph_continuous_p_binary()
    }
  })
  
  observeEvent(input$plot_specified_click_continuous_p_binary, {
    pt <- input$plot_specified_click_continuous_p_binary
    if(input$which_group == "group_0" & length(continuous_p_binary_group1$data) == 0){
      continuous_p_binary_group0$data <- c(continuous_p_binary_group0$data, pt$x, pt$y)
    }else if (input$which_group == "group_1" & length(continuous_p_binary_group0$data) == 0){
      continuous_p_binary_group1$data <- c(continuous_p_binary_group1$data, pt$x, pt$y)
    }
  })
  
  observeEvent(input$submit_continuous_p_binary, {
    submit_one_group$data <- TRUE
  })
  
  observeEvent(input$reset_continuous_p_binary_model, {
    continuous_p_binary_group0$data <- c()
    continuous_p_binary_group1$data <- c()
    submit_one_group$data <- FALSE
    output$result_continuous_binary <- renderText("")
    output$continuous_binary_equation <- renderTable(data.frame())
  })
  
  ########## continuous*binary ############
  
  continuous_m_binary_group0 <- reactiveValues(data = c())
  continuous_m_binary_group1 <- reactiveValues(data = c())
  submit_two_groups <- reactiveValues(data = FALSE)
  
  which_graph_continuous_m_binary <- reactive({
    group0 <- continuous_m_binary_group0$data
    group1 <- continuous_m_binary_group1$data
    plot_module2_cmb(group0, group1)
  })
  
  which_graph_continuous_m_binary_submit <- reactive({
    if(length(continuous_m_binary_group0$data) != 0 & length(continuous_m_binary_group1$data) == 0){
      output$result_continuous_m_binary <- renderText("Please create points for group 1 before submitting.")
      submit_two_groups$data <- FALSE
    }else if(length(continuous_m_binary_group0$data) == 0 & length(continuous_m_binary_group1$data) != 0){
      output$result_continuous_m_binary <- renderText("Please create points for group 0 before submitting.")
      submit_two_groups$data <- FALSE
    }else if(length(continuous_m_binary_group0$data) != 0 & length(continuous_m_binary_group1$data) != 0){
      data_group0 <- clean_to_dataframe(continuous_m_binary_group0$data)
      data_group0$d <- 0
      data_group1 <- clean_to_dataframe(continuous_m_binary_group1$data)
      data_group1$d <- 1
      data_full <- rbind(data_group0, data_group1)
      model <- lm(y~x*d, data = data_full)
      data_full$fitted.values <- model$fitted.values
      data_0 <- subset(data_full, d==0)
      data_1 <- subset(data_full, d==1)
     
      output$result_continuous_m_binary <- renderText(paste0("The linear model with one continuous predictor and one binary predictor and their interaction is y = b0 + b1x + b2d + b3x*d + e.
                                               The d is the binary variable indicating an observation belonging to group 0 or 1. Based on the data you submit, the parameters are: "))
      table <- data.frame(b0 = round(summary(model)$coefficients[1],2), b1 = round(summary(model)$coefficients[2],2), b2 = round(summary(model)$coefficients[3],2), b3 = round(summary(model)$coefficients[4],2), sigma = round(summary(model)$sigma, 2))
      output$continuous_m_binary_equation <- renderTable(table)
      
      data_full <- data_full %>% mutate(error = paste0("e", row_number()), error_sq = paste0("e", row_number(),"^2")) 
      if(nrow(data_full) <=4){
        sigma <- paste0("The degree of freedom is ",nrow(data_full) , " - 4, smaller or equal to 0, so sigma cannot be calculated.")
        formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + ", round(summary(model)$coefficients[3],2), "d + ",round(summary(model)$coefficients[4],2),  "x*d + e" )
      }else{
        sigma <- create_sigma_formula(data_full$error_sq[1], data_full$error_sq[nrow(data_full)], nrow(data_full), 4, round(summary(model)$sigma, 2))
        formula <- paste0("y = ", round(summary(model)$coefficients[1],2), " + ", round(summary(model)$coefficients[2],2), "x + ", round(summary(model)$coefficients[3],2), "d + ",round(summary(model)$coefficients[4],2),  "x*d + ","e, ", "e ~ N(0, ", round(summary(model)$sigma, 2),"^2)" )
      }
      plot_module2_cmb_submit(data_1, data_0, data_full, formula, sigma)
      
    }else{
      output$result_continuous_m_binary <- renderText("Please create points for both groups before submitting.")
      submit_two_groups$data <- FALSE
    }
  })
  
  output$plot_specified_continuous_m_binary <- renderPlot({
    if(submit_two_groups$data == TRUE){
      which_graph_continuous_m_binary_submit()
    }else{
      which_graph_continuous_m_binary()
    }
    
  })
  
  observeEvent(input$plot_specified_click_continuous_m_binary, {
    pt <- input$plot_specified_click_continuous_m_binary
    if(input$which_group_interaction == "group_0_interaction"){
      continuous_m_binary_group0$data <- c(continuous_m_binary_group0$data, pt$x, pt$y)
    }else if (input$which_group_interaction == "group_1_interaction"){
      continuous_m_binary_group1$data <- c(continuous_m_binary_group1$data, pt$x, pt$y)
    }
  })
  
  observeEvent(input$submit_continuous_m_binary, {
    submit_two_groups$data <- TRUE
  })
  
  observeEvent(input$reset_continuous_m_binary_model, {
    continuous_m_binary_group0$data <- c()
    continuous_m_binary_group1$data <- c()
    submit_two_groups$data <- FALSE
    output$result_continuous_m_binary <- renderText("")
    output$continuous_m_binary_equation <- renderTable(data.frame())
  })
  
}