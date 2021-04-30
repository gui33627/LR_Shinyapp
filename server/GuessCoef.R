

GuessCoef <- function(input, output, session){
  
  ################ continuous #################
  
  coefficient_assess_guess <- reactiveValues(b0 = 3, b1 = 1)
  residualsd_assess_guess <- reactiveValues(data = 2)
  submit_coef <- reactiveValues(data = FALSE)
  submit_sigma <- reactiveValues(data = FALSE)
  
  x <- seq(0,10, by = 0.1)
  y <- reactive({ x*coefficient_assess_guess$b1 + coefficient_assess_guess$b0})
  residual <- reactive({rnorm(length(x),0,residualsd_assess_guess$data)})
  
  which_graph_guess <- reactive({
    data_true <- data.frame(x = x, y = y(), y_sample = y() + residual())
    if (input$guess_type_assess == "guess_coef") {
      output$guess_result <- renderTable(data.frame())
      plot_module4_continuous(data_true = data_true, line = TRUE, submit = FALSE)
    }else{
      output$guess_result <- renderTable(data.frame())
      model <- lm(y_sample~x, data = data_true)
      data_true$fitted.values <- model$fitted.values
      plot_module4_continuous(data_true = data_true, line = FALSE, submit = FALSE)
    }
  })
  
  which_graph_guess_coef <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess$b0, " + ", coefficient_assess_guess$b1, "x + e")
    data_true <- data.frame(x = x, y = y(), y_sample = y() + residual())
    b0 <- input$guess_b0
    b1 <- input$guess_b1
    plot_module4_continuous(data_true = data_true, line = TRUE, submit = TRUE, formula = formula, b0 = b0, b1 = b1)
  })
  
  which_graph_guess_sigma <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess$b0, " + ", coefficient_assess_guess$b1, "x + e, e ~ N(0, ", residualsd_assess_guess$data, "^2)")
    data_true <- data.frame(x = x, y = y(), y_sample = y() + residual())
    residual_user <- rnorm(length(x),0,input$guess_sigma)
    data_true$y_sample_user <- data_true$y + residual_user
    model <- lm(y_sample~x, data = data_true)
    data_true$fitted.values <- model$fitted.values
    plot_module4_continuous(data_true = data_true, line = FALSE, submit = TRUE, formula = formula)
  })
  
  output$plot_assess_fix <- renderPlot({
    if (input$guess_type_assess == "guess_coef") {
      if(submit_coef$data == TRUE){
        which_graph_guess_coef()
      }else{
        which_graph_guess()
      }
    }else if(input$guess_type_assess == "guess_residual"){
      if(submit_sigma$data == TRUE){
        which_graph_guess_sigma()
      }else{
        which_graph_guess()
      }
    }
  })
  
  observeEvent(input$submit, {
    if(input$guess_type_assess == "guess_coef"){
      table <- create_table(c("b0", "b1"), c( coefficient_assess_guess$b0, coefficient_assess_guess$b1), c(input$guess_b0, input$guess_b1), coef = TRUE)
      output$guess_result <- renderTable(table)
      submit_coef$data <- TRUE
    }else{
      table <- data.frame( true_sigma = c(residualsd_assess_guess$data), your_sigma = c(input$guess_sigma))
      output$guess_result <- renderTable(table)
      submit_sigma$data <- TRUE
    }
  })
  
  
  observeEvent(input$reset_guess, {
    coefficient_assess_guess$b0 <- sample(c(1:3,7:9), 1)
    if(coefficient_assess_guess$b0 >= 5){
      coefficient_assess_guess$b1 <- sample(seq(-2, -0.5, by = 0.5),1)
    }else{
      coefficient_assess_guess$b1 <- sample(seq(0.5, 2, by = 0.5),1)
    }
    residualsd_assess_guess$data <- sample(seq(0.4,2.4, by = 0.2), 1)
    output$guess_result <- renderTable(data.frame())
    submit_coef$data <- FALSE
    submit_sigma$data <- FALSE
  })
  
  
  #################### binary ###########################
  
  coefficient_assess_guess_binary <- reactiveValues(b0 = 4, b1 = 2)
  residualsd_assess_guess_binary <- reactiveValues(data = 1)
  submit_coef_binary <- reactiveValues(data = FALSE)
  submit_sigma_binary <- reactiveValues(data = FALSE)
  
  d_binary <- c(rep(0,50), rep(1,50))
  y_binary <- reactive({d_binary*coefficient_assess_guess_binary$b1 + coefficient_assess_guess_binary$b0})
  residual_binary <- reactive({rnorm(length(d_binary),0,residualsd_assess_guess_binary$data)})
  
  which_graph_guess_binary <- reactive({
    data_true <- data.frame(d = d_binary, y = y_binary(), y_sample = y_binary() + residual_binary())
    if (input$guess_type_assess_binary == "guess_coef_binary") {
      output$guess_result_binary <- renderTable(data.frame())
      data_0 <- subset(data_true, d == 0)
      data_1 <- subset(data_true, d == 1)
      plot_module4_binary(mean = TRUE, submit = FALSE, data_0, data_1)
      
    }else{
      output$guess_result_binary <- renderTable(data.frame())
      model <- lm(y_sample~factor(d), data = data_true)
      data_true$fitted.values <- model$fitted.values
      data_0 <- subset(data_true, d == 0)
      data_1 <- subset(data_true, d == 1)
      plot_module4_binary(mean = FALSE, submit = FALSE, data_0, data_1)
    }
  })
  
  which_graph_guess_coef_binary <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_binary$b0, " + ", coefficient_assess_guess_binary$b1, "d + e")
    data_true <- data.frame(d = d_binary, y = y_binary(), y_sample = y_binary() + residual_binary())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    b0 <- input$guess_b0_binary
    b1 <- input$guess_b1_binary
    plot_module4_binary(mean = TRUE, submit = TRUE, data_0 = data_0, data_1 = data_1, b0 = b0, b1 = b1, formula = formula)
  })
  
  which_graph_guess_sigma_binary <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_binary$b0, " + ", coefficient_assess_guess_binary$b1, "d + e, e ~ N(0, ", residualsd_assess_guess_binary$data, "^2)")
    data_true <- data.frame(d = d_binary, y = y_binary(), y_sample = y_binary() + residual_binary())
    residual_user <- rnorm(length(d_binary),0,input$guess_sigma_binary)
    data_true$y_sample_user <- data_true$y + residual_user
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    plot_module4_binary(mean = FALSE, submit = TRUE, data_0 = data_0, data_1 = data_1, formula = formula)
  })
  
  output$plot_assess_fix_binary <- renderPlot({
    if (input$guess_type_assess_binary == "guess_coef_binary") {
      if(submit_coef_binary$data == TRUE){
        which_graph_guess_coef_binary()
      }else{
        which_graph_guess_binary()
      }
    }else if(input$guess_type_assess_binary == "guess_residual_binary"){
      if(submit_sigma_binary$data == TRUE){
        which_graph_guess_sigma_binary()
      }else{
        which_graph_guess_binary()
      }
    }
  })
  
  observeEvent(input$submit_guess_binary, {
    if(input$guess_type_assess_binary == "guess_coef_binary"){
      table <- create_table(c("b0", "b1"), c(coefficient_assess_guess_binary$b0, coefficient_assess_guess_binary$b1), c(input$guess_b0_binary,input$guess_b1_binary), coef = TRUE)
      output$guess_result_binary <- renderTable(table)
      submit_coef_binary$data <- TRUE
    }else{
      table <- data.frame(true_sigma = c(residualsd_assess_guess_binary$data), your_sigma = c(input$guess_sigma_binary))
      output$guess_result_binary <- renderTable(table)
      submit_sigma_binary$data <- TRUE
    }
  })
  
  observeEvent(input$reset_guess_binary, {
    coefficient_assess_guess_binary$b0 <- sample(2.5:7.5, 1)
    if(coefficient_assess_guess_binary$b0 >= 5){
      coefficient_assess_guess_binary$b1 <- sample(seq(-4, -0.5, by = 0.5),1)
    }else{
      coefficient_assess_guess_binary$b1 <- sample(seq(0.5, 4, by = 0.5),1)
    }
    residualsd_assess_guess_binary$data <- sample(seq(0.4,2.4, by = 0.2), 1)
    output$guess_result_binary <- renderTable(data.frame())
    submit_coef_binary$data <- FALSE
    submit_sigma_binary$data <- FALSE
  })
  
  #################### continuous + binary ###########################
  
  coefficient_assess_guess_cpb <- reactiveValues(b0 = 3, b1 = 1, b2 = 2)
  residualsd_assess_guess_cpb <- reactiveValues(data = 1.5)
  submit_coef_cpb <- reactiveValues(data = FALSE)
  submit_sigma_cpb <- reactiveValues(data = FALSE)
  
  d_cpb <- c(rep(0,100), rep(1,100))
  x_cpb <- c(seq(0,10,length.out = 100), seq(0,10, length.out = 100))
  y_cpb <- reactive({ d_cpb*coefficient_assess_guess_cpb$b2 + x_cpb*coefficient_assess_guess_cpb$b1 + coefficient_assess_guess_cpb$b0})
  residual_cpb <- reactive({rnorm(length(d_cpb), 0, residualsd_assess_guess_cpb$data)})
  
  
  which_graph_guess_cpb <- reactive({
    data_true <- data.frame(d = d_cpb, x = x_cpb, y = y_cpb(), y_sample = y_cpb() + residual_cpb())
    model <- lm(y_sample~ x + factor(d), data = data_true)
    data_true$fitted.values <- model$fitted.values
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    
    if (input$guess_type_assess_cpb == "guess_coef_cpb") {
      output$guess_result_cpb <- renderTable(data.frame())
      plot_module4_cpb(line = TRUE, submit = FALSE, data_0 = data_0, data_1 = data_1)
    }else{
      output$guess_result_cpb <- renderTable(data.frame())
      plot_module4_cpb(line = FALSE, submit = FALSE, data_0 = data_0, data_1 = data_1)
    }
  })
  
  which_graph_guess_coef_cpb <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_cpb$b0, " + ", coefficient_assess_guess_cpb$b1, "x + ", coefficient_assess_guess_cpb$b2, "d + e")
    data_true <- data.frame(d = d_cpb, x = x_cpb, y = y_cpb(), y_sample = y_cpb() + residual_cpb())
    model <- lm(y_sample ~ x + factor(d), data = data_true)
    data_true$fitted.values <- model$fitted.values
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    b0 <- input$guess_b0_cpb
    b1 <- input$guess_b1_cpb
    b2 <- input$guess_b2_cpb
    plot_module4_cpb(line = TRUE, submit = TRUE, data_0 = data_0, data_1 = data_1, formula = formula, b0 = b0, b1 = b1, b2 = b2)
  })
  
  which_graph_guess_sigma_cpb <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_cpb$b0, " + ", coefficient_assess_guess_cpb$b1, "x + ", coefficient_assess_guess_cpb$b2, "d + e, e ~ N(0, ", residualsd_assess_guess_cpb$data, "^2)")
    data_true <- data.frame(d = d_cpb, x = x_cpb, y = y_cpb(), y_sample = y_cpb() + residual_cpb())
    residual_user <- rnorm(length(d_cpb),0,input$guess_sigma_cpb)
    data_true$y_sample_user <- data_true$y + residual_user
    model <- lm(y_sample~ x + factor(d), data = data_true)
    data_true$fitted.values <- model$fitted.values
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    plot_module4_cpb(line = FALSE, submit = TRUE, data_0 = data_0, data_1 = data_1, formula = formula)
  })
  
  
  
  output$plot_assess_fix_cpb <- renderPlot({
    if (input$guess_type_assess_cpb == "guess_coef_cpb") {
      if(submit_coef_cpb$data == TRUE){
        which_graph_guess_coef_cpb()
      }else{
        which_graph_guess_cpb()
      }
    }else if(input$guess_type_assess_cpb == "guess_residual_cpb"){
      if(submit_sigma_cpb$data == TRUE){
        which_graph_guess_sigma_cpb()
      }else{
        which_graph_guess_cpb()
      }
    }
  })
  
  observeEvent(input$submit_cpb, {
    if(input$guess_type_assess_cpb == "guess_coef_cpb"){
      table <- create_table(c("b0", "b1", "b2"), c(coefficient_assess_guess_cpb$b0, coefficient_assess_guess_cpb$b1, coefficient_assess_guess_cpb$b2), 
                          c(input$guess_b0_cpb, input$guess_b1_cpb,input$guess_b2_cpb), coef = TRUE)
      output$guess_result_cpb <- renderTable(table)
      submit_coef_cpb$data <- TRUE
    }else{
      table <- data.frame( true_sigma = c(residualsd_assess_guess_cpb$data), your_sigma = c(input$guess_sigma_cpb))
      output$guess_result_cpb <- renderTable(table)
      submit_sigma_cpb$data <- TRUE
    }
  })
  
  observeEvent(input$reset_guess_cpb, {
    coefficient_assess_guess_cpb$b0 <- sample(c(1:3,7:9), 1)
    if(coefficient_assess_guess_cpb$b0 >= 5){
      coefficient_assess_guess_cpb$b1 <- sample(seq(-2, -0.5, by = 0.5),1)
      coefficient_assess_guess_cpb$b2 <- sample(c(-4:-0.5),1)
    }else{
      coefficient_assess_guess_cpb$b1 <- sample(seq(0.5, 2, by = 0.5),1)
      coefficient_assess_guess_cpb$b2 <- sample(c(0.5:4),1)
    }
    residualsd_assess_guess_cpb$data <- sample(seq(0.4,2.4, by = 0.2), 1)
    output$guess_result_cpb <- renderTable(data.frame())
    submit_coef_cpb$data <- FALSE
    submit_sigma_cpb$data <- FALSE
  })
  
  #################### continuous * binary ###########################
  
  coefficient_assess_guess_cmb <- reactiveValues(b0 = 3, b1 = 1, b2 = 2, b3 = 1)
  residualsd_assess_guess_cmb <- reactiveValues(data = 1.5)
  submit_coef_cmb <- reactiveValues(data = FALSE)
  submit_sigma_cmb <- reactiveValues(data = FALSE)
  
  d_cmb <- c(rep(0,100), rep(1,100))
  x_cmb <- c(seq(0,10,length.out = 100), seq(0,10, length.out = 100))
  y_cmb <- reactive({ x_cmb*d_cmb*coefficient_assess_guess_cmb$b3 + d_cmb*coefficient_assess_guess_cmb$b2 + x_cmb*coefficient_assess_guess_cmb$b1 + coefficient_assess_guess_cmb$b0})
  residual_cmb <- reactive({rnorm(length(d_cmb),0,residualsd_assess_guess_cmb$data)})
  
  which_graph_guess_cmb <- reactive({
    data_true <- data.frame(d = d_cmb, x = x_cmb, y = y_cmb(), y_sample = y_cmb() + residual_cmb())
    model <- lm(y_sample~ x*factor(d), data = data_true)
    data_true$fitted.values <- model$fitted.values
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    
    if (input$guess_type_assess_cmb == "guess_coef_cmb") {
      output$guess_result_cmb <- renderTable(data.frame())
      plot_module4_cmb(line = TRUE, submit = FALSE, data_0 = data_0, data_1 = data_1)
    }else{
      output$guess_result_cmb <- renderTable(data.frame())
      plot_module4_cmb(line = FALSE, submit = FALSE, data_0 = data_0, data_1 = data_1)
    }
  })
  
  which_graph_guess_coef_cmb <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_cmb$b0, " + ", coefficient_assess_guess_cmb$b1, "x + ", coefficient_assess_guess_cmb$b2, "d + ", coefficient_assess_guess_cmb$b3, "x*d + e")
    data_true <- data.frame(d = d_cmb, x = x_cmb, y = y_cmb(), y_sample = y_cmb() + residual_cmb())
    model <- lm(y_sample ~ x*factor(d), data = data_true)
    data_true$fitted.values <- model$fitted.values
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    b0 <- input$guess_b0_cmb
    b1 <- input$guess_b1_cmb
    b2 <- input$guess_b2_cmb
    b3 <- input$guess_b3_cmb
    plot_module4_cmb(line = TRUE, submit = TRUE, data_0 = data_0, data_1 = data_1, formula = formula, b0 = b0, b1 = b1, b2 = b2, b3 = b3)
  })
  
  which_graph_guess_sigma_cmb <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_cmb$b0, " + ", coefficient_assess_guess_cmb$b1, "x + ", coefficient_assess_guess_cmb$b2, "d + ", coefficient_assess_guess_cmb$b3, "x*d + e, e ~ N(0, ", residualsd_assess_guess_cmb$data, "^2)")
    data_true <- data.frame(d = d_cmb, x = x_cmb, y = y_cmb(), y_sample = y_cmb() + residual_cmb())
    residual_user <- rnorm(length(d_cmb),0,input$guess_sigma_cmb)
    data_true$y_sample_user <- data_true$y + residual_user
    model <- lm(y_sample ~ x*factor(d), data = data_true)
    data_true$fitted.values <- model$fitted.values
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    plot_module4_cmb(line = FALSE, submit = TRUE, data_0 = data_0, data_1 = data_1, formula = formula)
  })
  
  output$plot_assess_fix_cmb <- renderPlot({
    if (input$guess_type_assess_cmb == "guess_coef_cmb") {
      if(submit_coef_cmb$data == TRUE){
        which_graph_guess_coef_cmb()
      }else{
        which_graph_guess_cmb()
      }
    }else if(input$guess_type_assess_cmb == "guess_residual_cmb"){
      if(submit_sigma_cmb$data == TRUE){
        which_graph_guess_sigma_cmb()
      }else{
        which_graph_guess_cmb()
      }
    }
  })
  
  observeEvent(input$submit_cmb, {
    if(input$guess_type_assess_cmb == "guess_coef_cmb"){
      table <- create_table(c("b0", "b1", "b2", "b3"), c(coefficient_assess_guess_cmb$b0, coefficient_assess_guess_cmb$b1, coefficient_assess_guess_cmb$b2, coefficient_assess_guess_cmb$b3), 
                          c(input$guess_b0_cmb, input$guess_b1_cmb,input$guess_b2_cmb,input$guess_b3_cmb), coef = TRUE)
      output$guess_result_cmb <- renderTable(table)
      submit_coef_cmb$data <- TRUE
    }else{
      table <- data.frame( true_sigma = c(residualsd_assess_guess_cmb$data), your_sigma = c(input$guess_sigma_cmb))
      output$guess_result_cmb <- renderTable(table)
      submit_sigma_cmb$data <- TRUE
    }
  })
  
  observeEvent(input$reset_guess_cmb, {
    coefficient_assess_guess_cmb$b0 <- sample(c(3:7), 1)
    if(coefficient_assess_guess_cmb$b0 >= 5){
      coefficient_assess_guess_cmb$b1 <- sample(seq(-2, -0.5, by = 0.5),1)
      coefficient_assess_guess_cmb$b2 <- sample(c(-3.5:-0.5),1)
    }else{
      coefficient_assess_guess_cmb$b1 <- sample(seq(0.5, 2, by = 0.5),1)
      coefficient_assess_guess_cmb$b2 <- sample(c(0.5:3.5),1)
    }
    coefficient_assess_guess_cmb$b3 <- sample(seq(0.5, 2, by = 0.5),1)
    residualsd_assess_guess_cmb$data <- sample(seq(0.4,2.4, by = 0.2), 1)
    output$guess_result_cmb <- renderTable(data.frame())
    submit_coef_cmb$data <- FALSE
    submit_sigma_cmb$data <- FALSE
  })
}