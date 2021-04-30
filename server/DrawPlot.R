
DrawPlot <- function(input, output, session, plotlyEvent){
  
  ############### coef and sigma ###################
  ############### continuous ################# 
  
  intercept_assess_draw <- reactiveValues(data = 3)
  coefficient_assess_draw <- reactiveValues(data = 1)
  residualsd_assess_draw <- reactiveValues(data = 3)
  samplesize_assess_draw <- reactiveValues(data = 10)
  
  drawline <- reactiveValues(data = c())
  drawobs <- reactiveValues(data = c())
  drawline_done <- reactiveValues(data = FALSE)
  
  output$intercept_assess_draw <- renderText(paste0("Intercept (b0): ", intercept_assess_draw$data))
  output$coefficient_assess_draw <- renderText(paste0("Coefficient on x (b1): ", coefficient_assess_draw$data))
  output$residualsd_assess_draw <- renderText(paste0("Residual Std. Dev. (sigma): ", residualsd_assess_draw$data))
  output$samplesize_assess_draw <- renderText(paste0("Sample Size: ", samplesize_assess_draw$data))
  
  which_graph_assess_line <- reactive({
    output$assessment_result <- renderTable(data.frame())
    if(length(drawline$data) != 0){
      data_current <- clean_to_dataframe(drawline$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        slope <- (data_current$y[2] - data_current$y[1])/(data_current$x[2] - data_current$x[1])
        intercept <- data_current$y[1] - slope*data_current$x[1]
        table <- create_table( c("b0", "b1"), c(intercept_assess_draw$data, coefficient_assess_draw$data), c(round(intercept,2), round(slope,2)), coef = TRUE)
        output$assessment_result <- renderTable(table)
        drawline_done$data <- TRUE
      }
      plot_module3_continuous_line(data_current)
    }else{
      plot_module3_continuous_line(data_current = data.frame())
    }})
  
  which_graph_assess_points <- reactive({
    output$assessment_result <- renderTable(data.frame())
    formula <- paste0("y = ", intercept_assess_draw$data, " + ", coefficient_assess_draw$data, "x + e, ", "e ~ N(0, ",residualsd_assess_draw$data,"^2)")
    if(length(drawobs$data) != 0){
      data_current <- clean_to_dataframe(drawobs$data)
      data_current <- clean_specified_size(data_current, samplesize_assess_draw$data)
      
      if(nrow(data_current) == samplesize_assess_draw$data){
        model <- lm(y~x, data = data_current)
        data_current$fitted.values <- model$fitted.values
        table <- create_table(c("b0", "b1", "sigma"), c(intercept_assess_draw$data, coefficient_assess_draw$data, residualsd_assess_draw$data), 
                            c(round(summary(model)$coefficients[1],2), round(summary(model)$coefficients[2],2), round(summary(model)$sigma,2)), para = TRUE)
        output$assessment_result <- renderTable(table)
        plot_module3_continuous_points(data_current, formula, intercept_assess_draw$data, coefficient_assess_draw$data, samplesize = TRUE)
      }else{
        plot_module3_continuous_points(data_current, formula, intercept_assess_draw$data, coefficient_assess_draw$data, samplesize = FALSE)
      }
    }else{
      data_current <- data.frame()
      plot_module3_continuous_points(data_current, formula, intercept_assess_draw$data, coefficient_assess_draw$data, samplesize = FALSE)
    }
  })
  
  which_graph_assess_line_addtrue <- reactive({
    data_current <- clean_to_dataframe(drawline$data)
    data_current <- clean_specified_size(data_current, 2)
    formula <- paste0("y = ", intercept_assess_draw$data, " + ", coefficient_assess_draw$data, "x + e")
    plot_module3_continuous_line(data_current, done = TRUE, formula = formula, intercept_assess_draw = intercept_assess_draw$data, coefficient_assess_draw = coefficient_assess_draw$data)
  })
  
  output$plot_assess_draw <- renderPlot({
    if (input$draw_type_assess == "draw_line_assess") {
      if(drawline_done$data == TRUE){
        which_graph_assess_line_addtrue()
      }else{
        which_graph_assess_line()
      }
    }else if(input$draw_type_assess == "draw_points_assess"){
      which_graph_assess_points()
    }
  })
  
  observeEvent(input$reset_assess, {
    intercept_assess_draw$data <- sample(1:10, 1)
    if(intercept_assess_draw$data >= 5){
      coefficient_assess_draw$data <- sample(seq(-2, -0.5, by = 0.5),1)
    }else{
      coefficient_assess_draw$data <- sample(seq(0.5, 2, by = 0.5),1)
    }
    residualsd_assess_draw$data <- sample(seq(0,4, by = 0.1), 1)
    samplesize_assess_draw$data <- sample(4:20, 1)
    drawline$data <- c()
    drawobs$data <- c()
    output$assessment_result <- renderTable(data.frame())
    output$data_info <- renderTable(data.frame())
    drawline_done$data <- FALSE
  })
  
  observeEvent(input$plot_assess_click, {
    pt <- input$plot_assess_click
    if(input$draw_type_assess == "draw_line_assess"){
      drawline$data <- c(drawline$data, pt$x, pt$y)
    }else{
      drawobs$data <- c(drawobs$data, pt$x, pt$y)
    }
  })
  
  ############### binary ################# 
  
  intercept_assess_binary_draw <- reactiveValues(data = 4)
  coefficient_assess_binary_draw <- reactiveValues(data = 1.5)
  residualsd_assess_binary_draw <- reactiveValues(data = 2)
  samplesize_assess_binary_draw <- reactiveValues(data = 6)
  
  drawmean0 <- reactiveValues(data = c())
  drawmean1 <- reactiveValues(data = c())
  drawobs_binary <- reactiveValues(data = c())
  drawline_done_binary <- reactiveValues(data = FALSE)
  
  output$intercept_assess_draw_binary <- renderText(paste0("Intercept (b0): ", intercept_assess_binary_draw$data))
  output$coefficient_assess_draw_binary <- renderText(paste0("Coefficient on d (b1): ", coefficient_assess_binary_draw$data))
  output$residualsd_assess_draw_binary <- renderText(paste0("Residual Std. Dev. (sigma): ", residualsd_assess_binary_draw$data))
  output$samplesize_assess_draw_binary <- renderText(paste0("Sample Size: ", samplesize_assess_binary_draw$data," for each group"))
  
  which_graph_assess_line_binary <- reactive({
    output$assessment_result_binary <- renderTable(data.frame())
    if(length(drawmean0$data) != 0 & length(drawmean1$data) == 0){
      data_group0 <- clean_to_dataframe(drawmean0$data)
      plot_module3_binary_mean(empty = FALSE, data_group0 = data_group0)
    }else if(length(drawmean0$data) == 0 & length(drawmean1$data) != 0){
      data_group1 <- clean_to_dataframe(drawmean1$data)
      plot_module3_binary_mean(empty = FALSE, data_group1 = data_group1)
    }else if(length(drawmean0$data) != 0 & length(drawmean1$data) != 0){
      data_group0 <- clean_to_dataframe(drawmean0$data)
      data_group1 <- clean_to_dataframe(drawmean1$data)
      table <- create_table(c("b0", "b1"), c(intercept_assess_binary_draw$data, coefficient_assess_binary_draw$data), 
                          c(round(data_group0$y,2), round(data_group1$y - data_group0$y,2)), coef = TRUE)
      output$assessment_result_binary <- renderTable(table)
      drawline_done_binary$data <- TRUE
    }else{
      plot_module3_binary_mean(empty = TRUE)
    }})
  
  which_graph_assess_points_binary <- reactive({
    output$assessment_result_binary <- renderTable(data.frame())
    formula <- paste0("y = ", intercept_assess_binary_draw$data, " + ", coefficient_assess_binary_draw$data, "d + e, e ~ N(0, ", residualsd_assess_binary_draw$data, "^2)")
    if(length(drawobs_binary$data) != 0){
      data_current <- clean_to_dataframe(drawobs_binary$data)
      data_current$d <- ifelse(data_current$x < 0.1, 0, 1)
      data_0 <- subset(data_current, d == 0)
      data_1 <- subset(data_current, d == 1)
      data_0 <- clean_specified_size(data_0, samplesize_assess_binary_draw$data)
      data_1 <- clean_specified_size(data_1, samplesize_assess_binary_draw$data)
      
      if(nrow(data_0) == samplesize_assess_binary_draw$data & nrow(data_1) == samplesize_assess_binary_draw$data){
        model <- lm(y ~ factor(d), data = data_current)
        table <- create_table(c("b0", "b1", "sigma"), c(intercept_assess_binary_draw$data, coefficient_assess_binary_draw$data, residualsd_assess_binary_draw$data), 
                            c(round(summary(model)$coefficients[1],2), round(summary(model)$coefficients[2],2),round(summary(model)$sigma,2)), para = TRUE)
        output$assessment_result_binary <- renderTable(table)
        plot_module3_binary_points(data_0 = data_0, data_1 = data_1,formula = formula, intercept_assess_binary_draw = intercept_assess_binary_draw$data, coefficient_assess_binary_draw = coefficient_assess_binary_draw$data, samplesize = TRUE, empty = FALSE)
      }else{
        plot_module3_binary_points(data_0 = data_0, data_1 = data_1,formula = formula, intercept_assess_binary_draw = intercept_assess_binary_draw$data, coefficient_assess_binary_draw = coefficient_assess_binary_draw$data, samplesize = FALSE, empty = FALSE)
      }
    }else{
      plot_module3_binary_points(formula = formula, intercept_assess_binary_draw = intercept_assess_binary_draw$data, coefficient_assess_binary_draw = coefficient_assess_binary_draw$data, samplesize = FALSE, empty = TRUE)
    }
  })
  
  which_graph_assess_line_addtrue_binary <- reactive({
    formula <- paste0("y = ", intercept_assess_binary_draw$data, " + ", coefficient_assess_binary_draw$data, "d + e")
    data_group0 <- clean_to_dataframe(drawmean0$data)
    data_group1 <- clean_to_dataframe(drawmean1$data)
    plot_module3_binary_mean(empty = FALSE, data_group0 = data_group0, data_group1 = data_group1, formula = formula, intercept_assess_binary_draw = intercept_assess_binary_draw$data, coefficient_assess_binary_draw = coefficient_assess_binary_draw$data)
  })
  
  output$plot_assess_draw_binary <- renderPlot({
    if (input$draw_type_assess_binary == "draw_line_assess_binary") {
      if(drawline_done_binary$data == TRUE){
        which_graph_assess_line_addtrue_binary()
      }else{
        which_graph_assess_line_binary()
      }}else if(input$draw_type_assess_binary == "draw_points_assess_binary"){
        which_graph_assess_points_binary()
      }
  })
  
  observeEvent(input$reset_assess_binary, {
    intercept_assess_binary_draw$data <- sample(3:7, 1)
    if(intercept_assess_binary_draw$data >= 5){
      coefficient_assess_binary_draw$data <- sample(seq(-4.5, 2.5, by = 0.5),1)
    }else{
      coefficient_assess_binary_draw$data <- sample(seq(-2.5, 4.5, by = 0.5),1)
    }
    residualsd_assess_binary_draw$data <- sample(seq(0,4, by = 0.2), 1)
    samplesize_assess_binary_draw$data <- sample(5:12, 1)
    drawmean0$data <- c()
    drawmean1$data <- c()
    drawobs_binary$data <- c()
    output$assessment_result_binary <- renderTable(data.frame())
    output$assessment_result_binary_text <- renderText("")
    drawline_done_binary$data <- FALSE
  })
  
  observeEvent(input$plot_assess_click_binary, {
    pt <- input$plot_assess_click_binary
    if(input$draw_type_assess_binary == "draw_line_assess_binary"){
      if(pt$x < 0.1 & length(drawmean0$data) == 0){
        drawmean0$data <- c(drawmean0$data, pt$x, pt$y)
      }else if(pt$x > 0.9 & length(drawmean1$data) == 0){
        drawmean1$data <- c(drawmean1$data, pt$x, pt$y)
      }
    }else{
      if(pt$x < 0.1 | pt$x > 0.9){
        drawobs_binary$data <- c(drawobs_binary$data, pt$x, pt$y)
      }
    }
  })
  
  
  ############### binary + continuous ################# 
  
  intercept_assess_draw_cpb <- reactiveValues(data = 3)
  b1_assess_draw_cpb <- reactiveValues(data = 1)
  b2_assess_draw_cpb <- reactiveValues(data = 2)
  residualsd_assess_draw_cpb <- reactiveValues(data = 2)
  samplesize_assess_draw_cpb <- reactiveValues(data = 10)
  
  drawline_gr0_cpb <- reactiveValues(data = c())
  drawobs_gr0_cpb <- reactiveValues(data = c())
  drawline_gr1_cpb <- reactiveValues(data = c())
  drawobs_gr1_cpb <- reactiveValues(data = c())
  
  drawline0_done_cpb <- reactiveValues(data = FALSE)
  drawline1_done_cpb <- reactiveValues(data = FALSE)
  drawobs_done_cpb_true <- reactiveValues(data = FALSE)
  drawobs_done_cpb_false <- reactiveValues(data = FALSE)
  submit_moved_points <- reactiveValues(data = FALSE)
  submit_moved_line <- reactiveValues(data = FALSE)
  
  output$intercept_assess_draw_cpb <- renderText(paste0("Intercept (b0): ", intercept_assess_draw_cpb$data))
  output$coefficient_assess_draw_cpb_b1 <- renderText(paste0("Coefficient on x (b1): ", b1_assess_draw_cpb$data))
  output$coefficient_assess_draw_cpb_b2 <- renderText(paste0("Coefficient on d (b2): ", b2_assess_draw_cpb$data))
  output$residualsd_assess_draw_cpb <- renderText(paste0("Residual Std. Dev. (sigma): ", residualsd_assess_draw_cpb$data))
  output$samplesize_assess_draw_cpb <- renderText(paste0("Sample Size: ", samplesize_assess_draw_cpb$data, " for each group"))
  
  which_graph_assess_line_cpb <- reactive({
    output$assessment_result_cpb <- renderTable(data.frame())
    formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e")
    if(length(drawline_gr0_cpb$data) != 0 & length(drawline_gr1_cpb$data) == 0){
      data_current <- clean_to_dataframe(drawline_gr0_cpb$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline0_done_cpb$data <- TRUE
      }
      plot_module3_cpb_line(data_current = data_current, group0 = TRUE, group1 = FALSE, done0 = FALSE, done1 = FALSE, formula = formula)
    }else if(length(drawline_gr0_cpb$data) == 0 & length(drawline_gr1_cpb$data) != 0){
      data_current <- clean_to_dataframe(drawline_gr1_cpb$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline1_done_cpb$data <- TRUE
      }
      plot_module3_cpb_line(data_current = data_current, group0 = FALSE, group1 = TRUE, done0 = FALSE, done1 = FALSE, formula = formula)
    }else if(length(drawline_gr0_cpb$data) != 0 & length(drawline_gr1_cpb$data) != 0){
      output$assessment_result_cpb_text <- renderText("Please only create points for one group.")
      drawline_gr0_cpb$data <- c()
      drawline_gr1_cpb$data <- c()
    }else{
      plot_module3_cpb_line(data_current = data.frame(), group0 = FALSE, group1 = FALSE, done0 = FALSE, done1 = FALSE, formula = formula)
    }})
  
  
  which_graph_assess_line_done0 <- reactive({
    output$assessment_result_cpb_text <- renderText("")
    formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e")
    data_current <- clean_to_dataframe(drawline_gr0_cpb$data)
    data_current <- clean_specified_size(data_current, 2)
    data_other <- data.frame(x= data_current$x, y = data_current$y + input$move_cpb_line)
    plot_module3_cpb_line(data_current = data_current, group0 = TRUE, group1 = FALSE, done0 = TRUE, done1 = FALSE, formula = formula, data_other = data_other)
  })
  
  which_graph_assess_line_done1 <- reactive({
    output$assessment_result_cpb_text <- renderText("")
    formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e")
    data_current <- clean_to_dataframe(drawline_gr1_cpb$data)
    data_current <- clean_specified_size(data_current, 2)
    data_other <- data.frame(x= data_current$x, y = data_current$y + input$move_cpb_line)
    plot_module3_cpb_line(data_current = data_current, group0 = FALSE, group1 = TRUE, done0 = FALSE, done1 = TRUE, formula = formula, data_other = data_other)
  })
  
  which_graph_assess_points_cpb <- reactive({
    output$assessment_result_cpb <- renderTable(data.frame())
    formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e, e ~ N(0, ", residualsd_assess_draw_cpb$data, "^2)")
    if(length(drawobs_gr0_cpb$data) != 0 & length(drawobs_gr1_cpb$data) == 0){
      data_current <- clean_to_dataframe(drawobs_gr0_cpb$data)
      data_current <- clean_specified_size(data_current, samplesize_assess_draw_cpb$data)
      if(nrow(data_current) == samplesize_assess_draw_cpb$data){
        drawobs_done_cpb_true$data <- TRUE
      }
      plot_module3_cpb_points(data_current = data_current, group0 = TRUE, group1 = FALSE, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula)
      
    }else if(length(drawobs_gr0_cpb$data) == 0 & length(drawobs_gr1_cpb$data) != 0){
      data_current <- clean_to_dataframe(drawobs_gr1_cpb$data)
      data_current <- clean_specified_size(data_current, samplesize_assess_draw_cpb$data)
      if(nrow(data_current) == samplesize_assess_draw_cpb$data){
        drawobs_done_cpb_false$data <- TRUE
      }
      plot_module3_cpb_points(data_current = data_current, group0 = FALSE, group1 = TRUE, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula)
    }else if(length(drawobs_gr0_cpb$data) != 0 & length(drawobs_gr1_cpb$data) != 0){
      output$assessment_result_cpb_text <- renderText("Please only create points for one group.")
      drawobs_gr0_cpb$data <- c()
      drawobs_gr1_cpb$data <- c()
    }else{
      plot_module3_cpb_points(data_current = data.frame(), group0 = FALSE, group1 = FALSE, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula)
    }
  })
  
  data_the_other_group <- reactive({
    if(drawobs_done_cpb_true$data ==TRUE){
      data_0 <- clean_to_dataframe(drawobs_gr0_cpb$data)
      data_0 <- clean_specified_size(data_0, samplesize_assess_draw_cpb$data)
      data <- sample_othe_group(data = data_0)
    }else if(drawobs_done_cpb_false$data ==TRUE){
      data_1 <- clean_to_dataframe(drawobs_gr1_cpb$data)
      data_1 <- clean_specified_size(data_1, samplesize_assess_draw_cpb$data)
      data <- sample_othe_group(data = data_1)
    }
    data %>% select(x,y)
  })
  
  which_graph_assess_points_cpb_done0 <- reactive({
    output$assessment_result_cpb_text <- renderText("")
    data_0 <- clean_to_dataframe(drawobs_gr0_cpb$data)
    data_0 <- clean_specified_size(data_0, samplesize_assess_draw_cpb$data)
    data_1 <- data_the_other_group()
    data_1$y <- data_1$y + input$move_cpb
    formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e, e ~ N(0, ", residualsd_assess_draw_cpb$data, "^2)")
    plot_module3_cpb_points(data_0 = data_0, data_1 = data_1, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula, submit0 = TRUE, submit1 = FALSE)
  })
  
  which_graph_assess_points_cpb_done1 <- reactive({
    output$assessment_result_cpb_text <- renderText("")
    data_1 <- clean_to_dataframe(drawobs_gr1_cpb$data)
    data_1 <- clean_specified_size(data_1, samplesize_assess_draw_cpb$data)
    data_0 <- data_the_other_group()
    data_0$y <- data_0$y + input$move_cpb
    formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e, e ~ N(0, ", residualsd_assess_draw_cpb$data, "^2)")
    plot_module3_cpb_points(data_0 = data_0, data_1 = data_1, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula, submit0 = FALSE, submit1 = TRUE)
  })
  
  
  output$plot_assess_draw_cpb <- renderPlot({
    if(input$draw_type_assess_continuous_p_binary == "draw_line_assess_continuous_p_binary"){
      formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e")
      if(drawline0_done_cpb$data ==TRUE){
        if(submit_moved_line$data == TRUE ){
          data_current <- clean_to_dataframe(drawline_gr0_cpb$data)
          data_current <- clean_specified_size(data_current, 2)
          slope <- (data_current$y[2] -data_current$y[1])/(data_current$x[2] -data_current$x[1])
          intercept <- data_current$y[1] - slope*data_current$x[1]
          table <- create_table(c("b0", "b1", "b2"), c(intercept_assess_draw_cpb$data, b1_assess_draw_cpb$data, b2_assess_draw_cpb$data), 
                              c(round(intercept,2), round(slope,2), round(input$move_cpb_line,2)), coef = TRUE)
          output$assessment_result_cpb <- renderTable({table})
          data_other <- data.frame(x= data_current$x, y = data_current$y + input$move_cpb_line)
          plot_module3_cpb_line(data_current = data_current, group0 = TRUE, group1 = TRUE, done0 = TRUE, done1 = FALSE, formula = formula,
                                intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, data_other = data_other)
        }else{
          which_graph_assess_line_done0()
        }
      }else if(drawline1_done_cpb$data ==TRUE){
        if(submit_moved_line$data == TRUE){
          data_current <- clean_to_dataframe(drawline_gr1_cpb$data)
          data_current <- clean_specified_size(data_current, 2)
          slope <- (data_current$y[2] -data_current$y[1])/(data_current$x[2] -data_current$x[1])
          intercept <- data_current$y[1] - slope*data_current$x[1] + input$move_cpb_line
          table <- create_table(c("b0", "b1", "b2"), c(intercept_assess_draw_cpb$data, b1_assess_draw_cpb$data, b2_assess_draw_cpb$data), 
                              c(round(intercept,2), round(slope,2), round(-input$move_cpb_line,2)), coef = TRUE)
          output$assessment_result_cpb <- renderTable({table})
          data_other <- data.frame(x= data_current$x, y = data_current$y + input$move_cpb_line)
          plot_module3_cpb_line(data_current = data_current, group0 = TRUE, group1 = TRUE, done0 = FALSE, done1 = TRUE, formula = formula,
                                intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, data_other = data_other)
        }else{
          which_graph_assess_line_done1()
        }
      }else{
        which_graph_assess_line_cpb()
      }
    }else{
      formula <- paste0("y = ", intercept_assess_draw_cpb$data, " + ", b1_assess_draw_cpb$data, "x + ", b2_assess_draw_cpb$data, "d + e, e ~ N(0, ", residualsd_assess_draw_cpb$data, "^2)")
      if(drawobs_done_cpb_true$data ==TRUE){
        if(submit_moved_points$data == TRUE ){
          data_0 <- clean_to_dataframe(drawobs_gr0_cpb$data)
          data_0 <- clean_specified_size(data_0, samplesize_assess_draw_cpb$data)
          data_1 <- data_the_other_group()
          data_1$y <- data_1$y + input$move_cpb
          data_0$d <- 0
          data_1$d <- 1
          data_current <- rbind(data_0,data_1)
          model <- lm(y ~ x + factor(d), data = data_current)
          data_current$fitted.values <- model$fitted.values
          data_0_fit <- subset(data_current, d == 0) 
          data_1_fit <- subset(data_current, d == 1) 
          
          table <- create_table(c("b0", "b1", "b2", "sigma"), c(intercept_assess_draw_cpb$data, b1_assess_draw_cpb$data, b2_assess_draw_cpb$data, residualsd_assess_draw_cpb$data), 
                              c(round(summary(model)$coefficients[1],2), round(summary(model)$coefficients[2],2), round(summary(model)$coefficients[3],2), round(summary(model)$sigma,2)), para = TRUE)
          output$assessment_result_cpb <- renderTable({table})
          plot_module3_cpb_points(data_0 = data_0, data_1 = data_1, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula, 
                                  submit0 = TRUE, submit1 = TRUE, flag = 0, data_0_fit = data_0_fit, data_1_fit = data_1_fit)
        }else{
          which_graph_assess_points_cpb_done0()
        }
      }else if(drawobs_done_cpb_false$data ==TRUE){
        
        if(submit_moved_points$data == TRUE){
          data_1 <- clean_to_dataframe(drawobs_gr1_cpb$data)
          data_1 <- clean_specified_size(data_1, samplesize_assess_draw_cpb$data)
          data_0 <- data_the_other_group()
          data_0$y <- data_0$y + input$move_cpb
          data_0$d <- 0
          data_1$d <- 1
          data_current <- rbind(data_0,data_1)
          model <- lm(y~x + factor(d), data = data_current)
          data_current$fitted.values <- model$fitted.values
          data_0_fit <- subset(data_current, d == 0) 
          data_1_fit <- subset(data_current, d == 1) 
          
          table <- create_table(c("b0", "b1", "b2", "sigma"), c(intercept_assess_draw_cpb$data, b1_assess_draw_cpb$data, b2_assess_draw_cpb$data, residualsd_assess_draw_cpb$data), 
                              c(round(summary(model)$coefficients[1],2), round(summary(model)$coefficients[2],2), round(summary(model)$coefficients[3],2), round(summary(model)$sigma,2)), para = TRUE)
          output$assessment_result_cpb <- renderTable({table})
          plot_module3_cpb_points(data_0 = data_0, data_1 = data_1, intercept_assess_draw_cpb = intercept_assess_draw_cpb$data, b1_assess_draw_cpb = b1_assess_draw_cpb$data, b2_assess_draw_cpb = b2_assess_draw_cpb$data, formula = formula, 
                                  submit0 = TRUE, submit1 = TRUE, flag = 1, data_0_fit = data_0_fit, data_1_fit = data_1_fit)
        }else{
          which_graph_assess_points_cpb_done1()
        }
      }else{
        which_graph_assess_points_cpb()
      }
    }
  })
  
  observeEvent(input$submit_moved_points, {
    submit_moved_points$data <- TRUE 
  })
  
  observeEvent(input$submit_drag_coef, {
    submit_moved_line$data <- TRUE 
  })
  
  observeEvent(input$reset_assess_cpb, {
    intercept_assess_draw_cpb$data <- sample(3:7, 1)
    if(intercept_assess_draw_cpb$data >= 5){
      b1_assess_draw_cpb$data <- sample(seq(-2, -0.5, by = 0.5),1)
      b2_assess_draw_cpb$data <- sample(c(-3:-0.5),1)
    }else{
      b1_assess_draw_cpb$data <- sample(seq(0.5, 2, by = 0.5),1)
      b2_assess_draw_cpb$data <- sample(c(0.5:3),1)
    }
    residualsd_assess_draw_cpb$data <- sample(seq(0.4,2.4, by = 0.2), 1)
    samplesize_assess_draw_cpb$data <- sample(4:12, 1)
    output$assessment_result_cpb <- renderTable(data.frame())
    output$assessment_result_cpb_text <- renderText("")
    drawline0_done_cpb$data <- FALSE
    drawline1_done_cpb$data <- FALSE
    drawobs_done_cpb_true$data <- FALSE
    drawobs_done_cpb_false$data <- FALSE
    submit_moved_points$data <- FALSE 
    submit_moved_line$data <- FALSE 
    drawline_gr0_cpb$data <- c()
    drawline_gr1_cpb$data <- c()
    drawobs_gr0_cpb$data <- c()
    drawobs_gr1_cpb$data <- c()
  })
  
  observeEvent(input$plot_assess_click_cpb, {
    pt <- input$plot_assess_click_cpb
    if(input$draw_type_assess_continuous_p_binary == "draw_line_assess_continuous_p_binary" & input$which_group_cpb == "group_0_cpb"){
      drawline_gr0_cpb$data <- c(drawline_gr0_cpb$data, pt$x, pt$y)
    }else if(input$draw_type_assess_continuous_p_binary == "draw_line_assess_continuous_p_binary" & input$which_group_cpb == "group_1_cpb"){
      drawline_gr1_cpb$data <- c(drawline_gr1_cpb$data, pt$x, pt$y)
    }else if(input$draw_type_assess_continuous_p_binary == "draw_points_assess_continuous_p_binary" & input$which_group_cpb == "group_0_cpb"){
      drawobs_gr0_cpb$data <- c(drawobs_gr0_cpb$data, pt$x, pt$y)
    }else{
      drawobs_gr1_cpb$data <- c(drawobs_gr1_cpb$data, pt$x, pt$y)
    }
  })
  
  
  ############### binary * continuous ################# 
  
  intercept_assess_draw_cmb <- reactiveValues(data = 2)
  b1_assess_draw_cmb <- reactiveValues(data = 1)
  b2_assess_draw_cmb <- reactiveValues(data = 1)
  b3_assess_draw_cmb <- reactiveValues(data = 3)
  residualsd_assess_draw_cmb <- reactiveValues(data = 3)
  samplesize_assess_draw_cmb <- reactiveValues(data = 10)
  
  drawline_gr0_cmb <- reactiveValues(data = c())
  drawobs_gr0_cmb <- reactiveValues(data = c())
  drawline_gr1_cmb <- reactiveValues(data = c())
  drawobs_gr1_cmb <- reactiveValues(data = c())
  
  drawline0_done_cmb <- reactiveValues(data = FALSE)
  drawline1_done_cmb <- reactiveValues(data = FALSE)
  drawobs_done_cmb_true <- reactiveValues(data = FALSE)
  drawobs_done_cmb_false <- reactiveValues(data = FALSE)
  
  output$intercept_assess_draw_cmb <- renderText(paste0("Intercept (b0): ", intercept_assess_draw_cmb$data))
  output$coefficient_assess_draw_cmb_b1 <- renderText(paste0("Coefficient on x (b1): ", b1_assess_draw_cmb$data))
  output$coefficient_assess_draw_cmb_b2 <- renderText(paste0("Coefficient on d (b2): ", b2_assess_draw_cmb$data))
  output$coefficient_assess_draw_cmb_b3 <- renderText(paste0("Coefficient on x*d (b3): ", b3_assess_draw_cmb$data))
  output$residualsd_assess_draw_cmb <- renderText(paste0("Residual Std. Dev. (sigma): ", residualsd_assess_draw_cmb$data))
  output$samplesize_assess_draw_cmb <- renderText(paste0("Sample Size: ", samplesize_assess_draw_cmb$data, " for each group"))
  
  which_graph_assess_line_cmb <- reactive({
    output$assessment_result_cmb <- renderTable(data.frame())
    if(length(drawline_gr0_cmb$data) != 0 & length(drawline_gr1_cmb$data) == 0){
      data_current <- clean_to_dataframe(drawline_gr0_cmb$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline0_done_cmb$data <- TRUE
      }
      plot_module3_cmb_line(group0 = TRUE, group1 = FALSE, data_current = data_current)
    }else if(length(drawline_gr0_cmb$data) == 0 & length(drawline_gr1_cmb$data) != 0){
      data_current <- clean_to_dataframe(drawline_gr1_cmb$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline1_done_cmb$data <- TRUE
      }
      plot_module3_cmb_line(group0 = FALSE, group1 = TRUE, data_current = data_current)
    }else if(length(drawline_gr0_cmb$data) != 0 & length(drawline_gr1_cmb$data) != 0){
      data_0 <- clean_to_dataframe(drawline_gr0_cmb$data)
      data_1 <- clean_to_dataframe(drawline_gr1_cmb$data)
      data_0 <- clean_specified_size(data_0, 2)
      data_1 <- clean_specified_size(data_1, 2)
      if(nrow(data_0) == 2){
        drawline0_done_cmb$data <- TRUE
      }
      if(nrow(data_1) == 2){
        drawline1_done_cmb$data <- TRUE
      }
      plot_module3_cmb_line(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1)
    }else{
      plot_module3_cmb_line(group0 = FALSE, group1 = FALSE, data_current = data.frame())
    }})
  
  which_graph_assess_line_done0_cmb <- reactive({
    data_current <- clean_to_dataframe(drawline_gr0_cmb$data)
    data_current <- clean_specified_size(data_current, 2)
    plot_module3_cmb_line(group0 = TRUE, group1 = FALSE, data_current = data_current, done0 = TRUE)
  })
  
  which_graph_assess_line_done1_cmb <- reactive({
    data_current <- clean_to_dataframe(drawline_gr1_cmb$data)
    data_current <- clean_specified_size(data_current, 2)
    plot_module3_cmb_line(group0 = FALSE, group1 = TRUE, data_current = data_current, done1 = TRUE)
  })
  
  which_graph_assess_line_done0half1_cmb <- reactive({
    data_0 <- clean_to_dataframe(drawline_gr0_cmb$data)
    data_1 <- clean_to_dataframe(drawline_gr1_cmb$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- clean_specified_size(data_1, 2)
    plot_module3_cmb_line(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, done0 = TRUE,done1 = FALSE)
  })
  
  which_graph_assess_line_done1half0_cmb <- reactive({
    data_0 <- clean_to_dataframe(drawline_gr0_cmb$data)
    data_1 <- clean_to_dataframe(drawline_gr1_cmb$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- clean_specified_size(data_1, 2)
    plot_module3_cmb_line(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, done0 = FALSE,done1 = TRUE)
  })
  
  which_graph_assess_line_bothdone_cmb <- reactive({
    formula <- paste0("y = ", intercept_assess_draw_cmb$data, " + ", b1_assess_draw_cmb$data, "x + ", b2_assess_draw_cmb$data, "d + ", b3_assess_draw_cmb$data, "x*d + e")
    data_0 <- clean_to_dataframe(drawline_gr0_cmb$data)
    data_1 <- clean_to_dataframe(drawline_gr1_cmb$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- clean_specified_size(data_1, 2)
    slope0 <- (data_0$y[2]-data_0$y[1])/(data_0$x[2]-data_0$x[1])
    slope1 <- (data_1$y[2]-data_1$y[1])/(data_1$x[2]-data_1$x[1])
    
    table <- create_table(c("b0", "b1", "b2", "b3"), c(intercept_assess_draw_cmb$data, b1_assess_draw_cmb$data, b2_assess_draw_cmb$data, b3_assess_draw_cmb$data),
                        c(round(data_0$y[1] - slope0*data_0$x[1],2), round(slope0,2), round(data_1$y[1] - slope1*data_1$x[1] - (data_0$y[1] - slope0*data_0$x[1]),2), round(slope1-slope0,2)), coef = TRUE)
    output$assessment_result_cmb <- renderTable(table)
    plot_module3_cmb_line(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, done0 = TRUE,done1 = TRUE, 
                          intercept_assess_draw_cmb = intercept_assess_draw_cmb$data, b1_assess_draw_cmb = b1_assess_draw_cmb$data, b2_assess_draw_cmb = b2_assess_draw_cmb$data, b3_assess_draw_cmb = b3_assess_draw_cmb$data, formula = formula)
  })
  
  which_graph_assess_points_cmb <- reactive({
    output$assessment_result_cmb <- renderTable(data.frame())
    formula <- paste0("y = ", intercept_assess_draw_cmb$data, " + ", b1_assess_draw_cmb$data, "x + ", b2_assess_draw_cmb$data, "d + ", b3_assess_draw_cmb$data, "x*d + e, e ~ N(0, ", residualsd_assess_draw_cmb$data, "^2)")
    if(length(drawobs_gr0_cmb$data) != 0 & length(drawobs_gr1_cmb$data) == 0){
      data_current <- clean_to_dataframe(drawobs_gr0_cmb$data)
      data_current <- clean_specified_size(data_current, samplesize_assess_draw_cmb$data)
      plot_module3_cmb_points(group0 = TRUE, group1 = FALSE, data_current = data_current, formula = formula, intercept_assess_draw_cmb = intercept_assess_draw_cmb$data, b1_assess_draw_cmb = b1_assess_draw_cmb$data, b2_assess_draw_cmb = b2_assess_draw_cmb$data, b3_assess_draw_cmb = b3_assess_draw_cmb$data)
    }else if(length(drawobs_gr0_cmb$data) == 0 & length(drawobs_gr1_cmb$data) != 0){
      data_current <- clean_to_dataframe(drawobs_gr1_cmb$data)
      data_current <- clean_specified_size(data_current, samplesize_assess_draw_cmb$data)
      plot_module3_cmb_points(group0 = FALSE, group1 = TRUE, data_current = data_current, formula = formula, intercept_assess_draw_cmb = intercept_assess_draw_cmb$data, b1_assess_draw_cmb = b1_assess_draw_cmb$data, b2_assess_draw_cmb = b2_assess_draw_cmb$data, b3_assess_draw_cmb = b3_assess_draw_cmb$data)
    }else if(length(drawobs_gr0_cmb$data) != 0 & length(drawobs_gr1_cmb$data) != 0){
      data_0 <- clean_to_dataframe(drawobs_gr0_cmb$data)
      data_0 <- clean_specified_size(data_0, samplesize_assess_draw_cmb$data)
      data_1 <- clean_to_dataframe(drawobs_gr1_cmb$data)
      data_1 <- clean_specified_size(data_1, samplesize_assess_draw_cmb$data)
      if(nrow(data_0) == samplesize_assess_draw_cmb$data & nrow(data_1) == samplesize_assess_draw_cmb$data){
        data_0$d <- 0
        data_1$d <- 1
        data_current <- rbind(data_0,data_1)
        model <- lm(y~x*factor(d), data = data_current)
        
        table <- create_table(c("b0", "b1", "b2", "b3"), c(intercept_assess_draw_cmb$data, b1_assess_draw_cmb$data, b2_assess_draw_cmb$data, b3_assess_draw_cmb$data),
                            c(round(summary(model)$coefficients[1],2), round(summary(model)$coefficients[2],2), round(summary(model)$coefficients[3],2), round(summary(model)$coefficients[4],2)), coef = TRUE)
        output$assessment_result_cmb <- renderTable(table)
        drawobs_done_cmb_true$data <- TRUE
      }
      plot_module3_cmb_points(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, formula = formula, intercept_assess_draw_cmb = intercept_assess_draw_cmb$data, b1_assess_draw_cmb = b1_assess_draw_cmb$data, b2_assess_draw_cmb = b2_assess_draw_cmb$data, b3_assess_draw_cmb = b3_assess_draw_cmb$data)
    }else{
      plot_module3_cmb_points(group0 = FALSE, group1 = FALSE, data_current = data.frame(), formula = formula, intercept_assess_draw_cmb = intercept_assess_draw_cmb$data, b1_assess_draw_cmb = b1_assess_draw_cmb$data, b2_assess_draw_cmb = b2_assess_draw_cmb$data, b3_assess_draw_cmb = b3_assess_draw_cmb$data)
    }
  })
  
  which_graph_assess_points_cmb_done_true <- reactive({
    formula <- paste0("y = ", intercept_assess_draw_cmb$data, " + ", b1_assess_draw_cmb$data, "x + ", b2_assess_draw_cmb$data, "d + ", b3_assess_draw_cmb$data, "x*d + e, e ~ N(0, ", residualsd_assess_draw_cmb$data, "^2)")
    data_0 <- clean_to_dataframe(drawobs_gr0_cmb$data)
    data_0 <- clean_specified_size(data_0, samplesize_assess_draw_cmb$data)
    data_1 <- clean_to_dataframe(drawobs_gr1_cmb$data)
    data_1 <- clean_specified_size(data_1, samplesize_assess_draw_cmb$data)
    data_0$d <- 0
    data_1$d <- 1
    data_current <- rbind(data_0,data_1)
    model <- lm(y ~ x*factor(d), data = data_current)
    data_current$fitted.values <- model$fitted.values
    data_0_fit <- subset(data_current, d == 0) 
    data_1_fit <- subset(data_current, d == 1) 
    
    plot_module3_cmb_points(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, done = TRUE, data_0_fit = data_0_fit, data_1_fit = data_1_fit,
                            formula = formula, intercept_assess_draw_cmb = intercept_assess_draw_cmb$data, b1_assess_draw_cmb = b1_assess_draw_cmb$data, b2_assess_draw_cmb = b2_assess_draw_cmb$data, b3_assess_draw_cmb = b3_assess_draw_cmb$data)
  })
  
  output$plot_assess_draw_cmb <- renderPlot({
    if(input$draw_type_assess_continuous_m_binary == "draw_line_assess_continuous_m_binary"){
      if ( drawline0_done_cmb$data == TRUE & drawline1_done_cmb$data == FALSE) {
        if(length(drawline_gr1_cmb$data) == 0){
          which_graph_assess_line_done0_cmb()
        }else{
          data_1 <- clean_to_dataframe(drawline_gr1_cmb$data)
          data_1 <- clean_specified_size(data_1, 2)
          if(nrow(data_1) < 2){
            which_graph_assess_line_done0half1_cmb()
          }else{
            drawline1_done_cmb$data <- TRUE
          }
        }
      }else if(drawline0_done_cmb$data == FALSE & drawline1_done_cmb$data == TRUE){
        
        if(length(drawline_gr0_cmb$data) == 0){
          which_graph_assess_line_done1_cmb()
        }else{
          data_0 <- clean_to_dataframe(drawline_gr0_cmb$data)
          data_0 <- clean_specified_size(data_0, 2)
          if(nrow(data_0) < 2){
            which_graph_assess_line_done1half0_cmb()
          }else{
            drawline0_done_cmb$data <- TRUE
          }
        }
      }else if(drawline0_done_cmb$data == TRUE & drawline1_done_cmb$data == TRUE){
        which_graph_assess_line_bothdone_cmb()
      }else{
        which_graph_assess_line_cmb()
      }
    }else{
      if(drawobs_done_cmb_true$data ==TRUE){
        which_graph_assess_points_cmb_done_true()
      }else{
        which_graph_assess_points_cmb()
      }
    }
  })
  
  observeEvent(input$reset_assess_cmb, {
    intercept_assess_draw_cmb$data <- sample(1:9, 1)
    if(intercept_assess_draw_cmb$data >= 5){
      b1_assess_draw_cmb$data <- sample(seq(-2, -0.5, by = 0.5),1)
      b2_assess_draw_cmb$data <- sample(c(-3.5:-0.5),1)
    }else{
      b1_assess_draw_cmb$data <- sample(seq(0.5, 2, by = 0.5),1)
      b2_assess_draw_cmb$data <- sample(c(0.5:3.5),1)
    }
    b3_assess_draw_cmb$data <- sample(seq(0.5, 2, by = 0.5),1)
    residualsd_assess_draw_cmb$data <- sample(seq(0,4, by = 0.2), 1)
    samplesize_assess_draw_cmb$data <- sample(4:12, 1)
    drawline_gr0_cmb$data <- c()
    drawline_gr1_cmb$data <- c()
    drawobs_gr0_cmb$data <- c()
    drawobs_gr1_cmb$data <- c()
    output$assessment_result_cmb <- renderTable(data.frame())
    drawline0_done_cmb$data <- FALSE
    drawline1_done_cmb$data <- FALSE
    drawobs_done_cmb_true$data <- FALSE
  })
  
  observeEvent(input$plot_assess_click_cmb, {
    pt <- input$plot_assess_click_cmb
    if(input$draw_type_assess_continuous_m_binary == "draw_line_assess_continuous_m_binary" & input$which_group_cmb == "group_0_cmb"){
      drawline_gr0_cmb$data <- c(drawline_gr0_cmb$data, pt$x, pt$y)
    }else if(input$draw_type_assess_continuous_m_binary == "draw_line_assess_continuous_m_binary" & input$which_group_cmb == "group_1_cmb"){
      drawline_gr1_cmb$data <- c(drawline_gr1_cmb$data, pt$x, pt$y)
    }else if(input$draw_type_assess_continuous_m_binary == "draw_points_assess_continuous_m_binary" & input$which_group_cmb == "group_0_cmb"){
      drawobs_gr0_cmb$data <- c(drawobs_gr0_cmb$data, pt$x, pt$y)
    }else{
      drawobs_gr1_cmb$data <- c(drawobs_gr1_cmb$data, pt$x, pt$y)
    }
  })
  
  
  ############### best fit ###################
  
  ############## continuous ###################
  
  coefficient_assess_guess_bestfit <- reactiveValues(b0 = 3, b1 = 1)
  residualsd_assess_guess_bestfit <- reactiveValues(data = 2)
  
  x_continuous_bestfit <- seq(0,10,by = 0.1)
  y_continuous_bestfit <- reactive({ x_continuous_bestfit*coefficient_assess_guess_bestfit$b1 + coefficient_assess_guess_bestfit$b0})
  residual_continuous_bestfit <- reactive({rnorm(length(x_continuous_bestfit),0,residualsd_assess_guess_bestfit$data)})
  
  drawline_bestfit <- reactiveValues(data = c())
  drawline_done_bestfit <- reactiveValues(data = FALSE)
  
  which_graph_assess_line_bestfit <- reactive({
    data_true <- data.frame(x = x_continuous_bestfit, y = y_continuous_bestfit(), y_sample = y_continuous_bestfit() + residual_continuous_bestfit())
    if(length(drawline_bestfit$data) != 0){
      data_current <- clean_to_dataframe(drawline_bestfit$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        slope <- (data_current$y[2] - data_current$y[1])/(data_current$x[2] - data_current$x[1])
        intercept <- data_current$y[1] - slope*data_current$x[1]
        table <- create_table(c("b0", "b1"), c(coefficient_assess_guess_bestfit$b0, coefficient_assess_guess_bestfit$b1), c(round(intercept,2), round(slope,2)), coef = TRUE)
        output$assessment_result_bestfit <- renderTable(table)
        drawline_done_bestfit$data <- TRUE
      }
      plot_module3_bestfit_continuous(data_current = data_current, data_true = data_true)
    }else{
      plot_module3_bestfit_continuous(data_current = data.frame(), data_true = data_true)
    }})
  
  
  
  which_graph_assess_line_addtrue_bestfit <- reactive({
    formula <- paste0("y = ",coefficient_assess_guess_bestfit$b0, " + ", coefficient_assess_guess_bestfit$b1,"x + e, e ~ N(0, ", residualsd_assess_guess_bestfit$data, "^2)")
    data_true <- data.frame(x = x_continuous_bestfit, y = y_continuous_bestfit(), y_sample = y_continuous_bestfit() + residual_continuous_bestfit())
    data_current <- clean_to_dataframe(drawline_bestfit$data)
    data_current <- data_current[1:2,]
    model <- lm(y_sample ~ x, data = data_true)
    data_true$fitted.values <- model$fitted.values
    plot_module3_bestfit_continuous(data_current = data_current, data_true = data_true, formula = formula)
  })
  
  output$plot_assess_draw_bestfit <- renderPlot({
    if(drawline_done_bestfit$data == TRUE){
      which_graph_assess_line_addtrue_bestfit()
    }else{
      which_graph_assess_line_bestfit()
    }
  })
  
  observeEvent(input$reset_assess_bestfit, {
    coefficient_assess_guess_bestfit$b0 <- sample(1:9, 1)
    if(coefficient_assess_guess_bestfit$b0 >= 5){
      coefficient_assess_guess_bestfit$b1 <- sample(seq(-2, -0.5, by = 0.5),1)
    }else{
      coefficient_assess_guess_bestfit$b1 <- sample(seq(0.5, 2, by = 0.5),1)
    }
    residualsd_assess_guess_bestfit$data <- sample(seq(0.5,3, by = 0.5), 1)
    output$assessment_result_bestfit <- renderTable(data.frame())
    drawline_done_bestfit$data <- FALSE
    drawline_bestfit$data <- c()
  })
  
  
  observeEvent(input$plot_assess_click_bestfit, {
    pt <- input$plot_assess_click_bestfit
    drawline_bestfit$data <- c(drawline_bestfit$data, pt$x, pt$y)
  })
  
  #################### binary ###########################
  
  coefficient_assess_guess_binary_bestfit <- reactiveValues(b0 = 4, b1 = 2)
  residualsd_assess_guess_binary_bestfit <- reactiveValues(data = 1)
  
  drawmean0_bestfit <- reactiveValues(data = c())
  drawmean1_bestfit <- reactiveValues(data = c())
  drawline_done_binary_bestfit <- reactiveValues(data = FALSE)
  
  d_binary_bestfit <- c(rep(0,50), rep(1,50))
  y_binary_bestfit <- reactive({ d_binary_bestfit*coefficient_assess_guess_binary_bestfit$b1 + coefficient_assess_guess_binary_bestfit$b0})
  residual_binary_bestfit <- reactive({rnorm(length(d_binary_bestfit),0,residualsd_assess_guess_binary_bestfit$data)})
  
  which_graph_assess_line_binary_bestfit <- reactive({
    data_true <- data.frame(d = d_binary_bestfit, y = y_binary_bestfit(), y_sample = y_binary_bestfit() + residual_binary_bestfit())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    
    if(length(drawmean0_bestfit$data) != 0 & length(drawmean1_bestfit$data) == 0){
      data_group0 <- clean_to_dataframe(drawmean0_bestfit$data)
      plot_module3_bestfit_binary(group0 = TRUE, group1 = FALSE, data_group0 = data_group0, data_0 = data_0, data_1 = data_1)
      
    }else if(length(drawmean0_bestfit$data) == 0 & length(drawmean1_bestfit$data) != 0){
      data_group1 <- clean_to_dataframe(drawmean1_bestfit$data)
      plot_module3_bestfit_binary(group0 = FALSE, group1 = TRUE, data_group1 = data_group1, data_0 = data_0, data_1 = data_1)
      
    }else if(length(drawmean0_bestfit$data) != 0 & length(drawmean1_bestfit$data) != 0){
      data_group0 <- clean_to_dataframe(drawmean0_bestfit$data)
      data_group1 <- clean_to_dataframe(drawmean1_bestfit$data)
      table <- create_table(c("b0", "b1"), c(coefficient_assess_guess_binary_bestfit$b0, coefficient_assess_guess_binary_bestfit$b1), 
                          c(round(data_group0$y,2), round(data_group1$y - data_group0$y,2)), coef = TRUE)
      output$assessment_result_binary_bestfit <- renderTable(table)
      drawline_done_binary_bestfit$data <- TRUE
    }else{
      plot_module3_bestfit_binary(group0 = FALSE, group1 = FALSE, data_0 = data_0, data_1 = data_1)
    }})
  
  which_graph_assess_line_addtrue_binary_bestfit <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_binary_bestfit$b0, " + ", coefficient_assess_guess_binary_bestfit$b1, "d + e, e ~ N(0, ",residualsd_assess_guess_binary_bestfit$data, "^2)")
    data_true <- data.frame(d = d_binary_bestfit, y = y_binary_bestfit(), y_sample = y_binary_bestfit() + residual_binary_bestfit())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    data_group0 <- clean_to_dataframe(drawmean0_bestfit$data)
    data_group1 <- clean_to_dataframe(drawmean1_bestfit$data)
    plot_module3_bestfit_binary(group0 = TRUE, group1 = TRUE, data_group0 = data_group0, data_group1 = data_group1, data_0 = data_0, data_1 = data_1, formula = formula)
  })
  
  output$plot_assess_draw_binary_bestfit <- renderPlot({
    if(drawline_done_binary_bestfit$data == TRUE){
      which_graph_assess_line_addtrue_binary_bestfit()
    }else{
      which_graph_assess_line_binary_bestfit()
    }
  })
  
  observeEvent(input$reset_assess_binary_bestfit, {
    coefficient_assess_guess_binary_bestfit$b0 <- sample(2.5:5.5, 1)
    if(coefficient_assess_guess_binary_bestfit$b0 >=5){
      coefficient_assess_guess_binary_bestfit$b1 <- sample(seq(-4.5, -0.5, by = 0.5),1)
    }else{
      coefficient_assess_guess_binary_bestfit$b1 <- sample(seq(0.5, 4.5, by = 0.5),1)
    }
    residualsd_assess_guess_binary_bestfit$data <- sample(seq(0.2,2, by = 0.2), 1)
    drawmean0_bestfit$data <- c()
    drawmean1_bestfit$data <- c()
    output$assessment_result_binary_bestfit <- renderTable(data.frame())
    drawline_done_binary_bestfit$data <- FALSE
  })
  
  
  observeEvent(input$plot_assess_click_binary_bestfit, {
    pt <- input$plot_assess_click_binary_bestfit
    if(pt$x < 0.1 & length(drawmean0_bestfit$data) == 0){
      drawmean0_bestfit$data <- c(drawmean0_bestfit$data, pt$x, pt$y)
    }else if(pt$x > 0.9 & length(drawmean1_bestfit$data) == 0){
      drawmean1_bestfit$data <- c(drawmean1_bestfit$data, pt$x, pt$y)
    }
  })
  
  ############### binary + continuous ################# 
  
  coefficient_assess_guess_cpb_bestfit <- reactiveValues(b0 = 1, b1 = 1, b2 = 2)
  residualsd_assess_guess_cpb_bestfit <- reactiveValues(data = 1)
  
  d_cpb_bestfit <- c(rep(0,100), rep(1,100))
  x_cpb_bestfit <- c(seq(0,10,length.out = 100), seq(0,10, length.out = 100))
  y_cpb_bestfit <- reactive({ d_cpb_bestfit*coefficient_assess_guess_cpb_bestfit$b2 + x_cpb_bestfit*coefficient_assess_guess_cpb_bestfit$b1 + coefficient_assess_guess_cpb_bestfit$b0})
  residual_cpb_bestfit <- reactive({rnorm(length(d_cpb_bestfit),0,residualsd_assess_guess_cpb_bestfit$data)})
  
  drawline_gr0_cpb_bestfit <- reactiveValues(data = c())
  drawline_gr1_cpb_bestfit <- reactiveValues(data = c())
  
  drawline0_done_cpb_bestfit <- reactiveValues(data = FALSE)
  drawline1_done_cpb_bestfit <- reactiveValues(data = FALSE)
  submit_line_bestfit <- reactiveValues(data = FALSE)
  
  which_graph_assess_line_cpb_bestfit <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_cpb_bestfit$b0, " + ", coefficient_assess_guess_cpb_bestfit$b1, "x + ", coefficient_assess_guess_cpb_bestfit$b2, "d + e, e ~ N(0, ", residualsd_assess_guess_cpb_bestfit$data, "^2)")
    data_true <- data.frame(d = d_cpb_bestfit, x = x_cpb_bestfit, y = y_cpb_bestfit(), y_sample = y_cpb_bestfit() + residual_cpb_bestfit())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    
    if(length(drawline_gr0_cpb_bestfit$data) != 0 & length(drawline_gr1_cpb_bestfit$data) == 0){
      data_current <- clean_to_dataframe(drawline_gr0_cpb_bestfit$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline0_done_cpb_bestfit$data <- TRUE
      }
      plot_module3_bestfit_cpb(group0 = TRUE, group1 = FALSE,data_current = data_current, data_0 = data_0, data_1 = data_1, done0 = FALSE, done1 = FALSE, formula = formula)
      
    }else if(length(drawline_gr0_cpb_bestfit$data) == 0 & length(drawline_gr1_cpb_bestfit$data) != 0){
      data_current <- clean_to_dataframe(drawline_gr1_cpb_bestfit$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline1_done_cpb_bestfit$data <- TRUE
      }
      plot_module3_bestfit_cpb(group0 = FALSE, group1 = TRUE,data_current = data_current, data_0 = data_0, data_1 = data_1, done0 = FALSE, done1 = FALSE, formula = formula)
      
    }else if(length(drawline_gr0_cpb_bestfit$data) != 0 & length(drawline_gr1_cpb_bestfit$data) != 0){
      output$assessment_result_cpb_bestfit <- renderText("Please create a line for only one group.")
      drawline_gr0_cpb_bestfit$data <- c()
      drawline_gr1_cpb_bestfit$data <- c()
    }else{
      plot_module3_bestfit_cpb(group0 = FALSE, group1 = FALSE,data_0 = data_0, data_1 = data_1, formula = formula)
    }})
  
  which_graph_assess_line_done0_bestfit <- reactive({
    output$assessment_result_cpb_bestfit <- renderText("")
    formula <- paste0("y = ", coefficient_assess_guess_cpb_bestfit$b0, " + ", coefficient_assess_guess_cpb_bestfit$b1, "x + ", coefficient_assess_guess_cpb_bestfit$b2, "d + e, e ~ N(0, ", residualsd_assess_guess_cpb_bestfit$data, "^2)")
    data_true <- data.frame(d = d_cpb_bestfit, x = x_cpb_bestfit, y = y_cpb_bestfit(), y_sample = y_cpb_bestfit() + residual_cpb_bestfit())
    data_gr0 <- subset(data_true, d == 0)
    data_gr1 <- subset(data_true, d == 1)
    data_0 <- clean_to_dataframe(drawline_gr0_cpb_bestfit$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- data.frame(x = data_0$x, y = data_0$y + input$move_cpb_line_bestfit)
    plot_module3_bestfit_cpb(group0 = TRUE, group1 = FALSE, data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, done0 = TRUE, done1 = FALSE, formula = formula)
  })
  
  which_graph_assess_line_done1_bestfit <- reactive({
    output$assessment_result_cpb_bestfit <- renderText("")
    formula <- paste0("y = ", coefficient_assess_guess_cpb_bestfit$b0, " + ", coefficient_assess_guess_cpb_bestfit$b1, "x + ", coefficient_assess_guess_cpb_bestfit$b2, "d + e, e ~ N(0, ", residualsd_assess_guess_cpb_bestfit$data, "^2)")
    data_true <- data.frame(d = d_cpb_bestfit, x = x_cpb_bestfit, y = y_cpb_bestfit(), y_sample = y_cpb_bestfit() + residual_cpb_bestfit())
    data_gr0 <- subset(data_true, d == 0)
    data_gr1 <- subset(data_true, d == 1)
    data_1 <- clean_to_dataframe(drawline_gr1_cpb_bestfit$data)
    data_1 <- clean_specified_size(data_1, 2)
    data_0 <- data.frame(x = data_1$x, y = data_1$y + input$move_cpb_line_bestfit)
    plot_module3_bestfit_cpb(group0 = FALSE, group1 = TRUE, data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, done0 = FALSE, done1 = TRUE, formula = formula)
  })
  
  output$plot_assess_draw_cpb_bestfit <- renderPlot({
    data_true <- data.frame(d = d_cpb_bestfit, x = x_cpb_bestfit, y = y_cpb_bestfit(), y_sample = y_cpb_bestfit() + residual_cpb_bestfit())
    data_gr0 <- subset(data_true, d == 0)
    data_gr1 <- subset(data_true, d == 1)
    formula <- paste0("y = ", coefficient_assess_guess_cpb_bestfit$b0, " + ", coefficient_assess_guess_cpb_bestfit$b1, "x + ", coefficient_assess_guess_cpb_bestfit$b2, "d + e, e ~ N(0, ", residualsd_assess_guess_cpb_bestfit$data, "^2)")
    
    if ( drawline0_done_cpb_bestfit$data == TRUE & drawline1_done_cpb_bestfit$data == FALSE) {
      if(submit_line_bestfit$data == FALSE){
        which_graph_assess_line_done0_bestfit()
      }else{
        data_0 <- clean_to_dataframe(drawline_gr0_cpb_bestfit$data)
        data_0 <- clean_specified_size(data_0, 2)
        data_1 <- data.frame(x = data_0$x, y = data_0$y + input$move_cpb_line_bestfit)
        plot_module3_bestfit_cpb(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, 
                                 done0 = TRUE, done1 = TRUE, b0 = coefficient_assess_guess_cpb_bestfit$b0, b1 = coefficient_assess_guess_cpb_bestfit$b1, b2 = coefficient_assess_guess_cpb_bestfit$b2, formula = formula)
      }
    }else if(drawline0_done_cpb_bestfit$data == FALSE & drawline1_done_cpb_bestfit$data == TRUE){
      if(submit_line_bestfit$data == FALSE){
        which_graph_assess_line_done1_bestfit()
      }else{
        data_1 <- clean_to_dataframe(drawline_gr1_cpb_bestfit$data)
        data_1 <- clean_specified_size(data_1, 2)
        data_0 <- data.frame(x = data_1$x, y = data_1$y + input$move_cpb_line_bestfit)
        plot_module3_bestfit_cpb(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, 
                                 done0 = TRUE, done1 = TRUE, b0 = coefficient_assess_guess_cpb_bestfit$b0, b1 = coefficient_assess_guess_cpb_bestfit$b1, b2 = coefficient_assess_guess_cpb_bestfit$b2, formula = formula)
      }
    }else if(drawline0_done_cpb_bestfit$data == FALSE & drawline1_done_cpb_bestfit$data == FALSE){
      which_graph_assess_line_cpb_bestfit()
    }
  })
  
  observeEvent(input$submit_drag,{
    submit_line_bestfit$data <- TRUE
    output$drag <- renderTable({
      truedata <- c(coefficient_assess_guess_cpb_bestfit$b0, coefficient_assess_guess_cpb_bestfit$b1, coefficient_assess_guess_cpb_bestfit$b2)
      if ( drawline0_done_cpb_bestfit$data == TRUE & drawline1_done_cpb_bestfit$data == FALSE) {
        data_current <- clean_to_dataframe(drawline_gr0_cpb_bestfit$data)
        data_current <- clean_specified_size(data_current, 2)
        slope <- round((data_current$y[2]-data_current$y[1])/(data_current$x[2]-data_current$x[1]),2)
        intercept <- round(data_current$y[1] - slope*data_current$x[1],2)
        data <- create_table(c("b0","b1","b2"), truedata, c(intercept, slope, input$move_cpb_line_bestfit), coef = TRUE)
        data
        
      }else if(drawline0_done_cpb_bestfit$data == FALSE & drawline1_done_cpb_bestfit$data == TRUE){
        data_current <- clean_to_dataframe(drawline_gr1_cpb_bestfit$data)
        data_current <- clean_specified_size(data_current, 2)
        slope <- round((data_current$y[2]-data_current$y[1])/(data_current$x[2]-data_current$x[1]),2)
        intercept <- round(data_current$y[1] - slope*data_current$x[1] + input$move_cpb_line_bestfit,2)
        data <- create_table(c("b0","b1","b2"), truedata, c(intercept, slope, -input$move_cpb_line_bestfit), coef = TRUE)
        data
      }else{
        data.frame(true_coefficient = c(), your_coefficient = c())
      }
    })
  })
  
  observeEvent(input$reset_assess_cpb_bestfit, {
    coefficient_assess_guess_cpb_bestfit$b0 <- sample(2:8, 1)
    if(coefficient_assess_guess_cpb_bestfit$b0 >= 5){
      coefficient_assess_guess_cpb_bestfit$b1 <- sample(seq(-2, -0.5, by = 0.5),1)
      coefficient_assess_guess_cpb_bestfit$b2 <- sample(seq(-2.5, -0.5, by = 0.5),1)
    }else{
      coefficient_assess_guess_cpb_bestfit$b1 <- sample(seq(0.5, 2, by = 0.5),1)
      coefficient_assess_guess_cpb_bestfit$b2 <- sample(seq(0.5, 2.5, by = 0.5),1)
    }
    residualsd_assess_guess_cpb_bestfit$data <- sample(seq(0.5,3, by = 0.5), 1)
    output$drag <- renderTable({data.frame() })
    submit_line_bestfit$data <- FALSE
    drawline_gr0_cpb_bestfit$data <- c()
    drawline_gr1_cpb_bestfit$data <- c()
    output$assessment_result_cpb_bestfit <- renderText("")
    drawline0_done_cpb_bestfit$data <- FALSE
    drawline1_done_cpb_bestfit$data <- FALSE
  })
  
  observeEvent(input$plot_assess_click_cpb_bestfit, {
    pt <- input$plot_assess_click_cpb_bestfit
    if(input$which_group_cpb_bestfit == "group_0_cpb_bestfit"){
      drawline_gr0_cpb_bestfit$data <- c(drawline_gr0_cpb_bestfit$data, pt$x, pt$y)
    }else if(input$which_group_cpb_bestfit == "group_1_cpb_bestfit"){
      drawline_gr1_cpb_bestfit$data <- c(drawline_gr1_cpb_bestfit$data, pt$x, pt$y)
    }
  })
  
  ############### binary * continuous ################# 
  
  coefficient_assess_guess_cmb_bestfit <- reactiveValues(b0 = 1, b1 = 1, b2 = 2, b3 = 1)
  residualsd_assess_guess_cmb_bestfit <- reactiveValues(data = 1.5)
  
  d_cmb_bestfit <- c(rep(0,100), rep(1,100))
  x_cmb_bestfit <- c(seq(0,10,length.out = 100), seq(0,10, length.out = 100))
  y_cmb_bestfit <- reactive({ x_cmb_bestfit*d_cmb_bestfit*coefficient_assess_guess_cmb_bestfit$b3 + d_cmb_bestfit*coefficient_assess_guess_cmb_bestfit$b2 + x_cmb_bestfit*coefficient_assess_guess_cmb_bestfit$b1 + coefficient_assess_guess_cmb_bestfit$b0})
  residual_cmb_bestfit <- reactive({rnorm(length(d_cmb_bestfit),0,residualsd_assess_guess_cmb_bestfit$data)})
  
  drawline_gr0_cmb_bestfit <- reactiveValues(data = c())
  drawline_gr1_cmb_bestfit <- reactiveValues(data = c())
  drawline0_done_cmb_bestfit <- reactiveValues(data = FALSE)
  drawline1_done_cmb_bestfit <- reactiveValues(data = FALSE)
  
  which_graph_assess_line_cmb_bestfit <- reactive({
    data_true <- data.frame(d = d_cmb_bestfit, x = x_cmb_bestfit, y = y_cmb_bestfit(), y_sample = y_cmb_bestfit() + residual_cmb_bestfit())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    
    if(length(drawline_gr0_cmb_bestfit$data) != 0 & length(drawline_gr1_cmb_bestfit$data) == 0){
      data_current <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline0_done_cmb_bestfit$data <- TRUE
      }
      plot_module3_bestfit_cmb(group0 = TRUE, group1 = FALSE,data_current = data_current, data_0 = data_0, data_1 = data_1)
    }else if(length(drawline_gr0_cmb_bestfit$data) == 0 & length(drawline_gr1_cmb_bestfit$data) != 0){
      data_current <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
      data_current <- clean_specified_size(data_current, 2)
      if(nrow(data_current) == 2){
        drawline1_done_cmb_bestfit$data <- TRUE
      }
      plot_module3_bestfit_cmb(group0 = FALSE, group1 = TRUE,data_current = data_current, data_0 = data_0, data_1 = data_1)
    }else if(length(drawline_gr0_cmb_bestfit$data) != 0 & length(drawline_gr1_cmb_bestfit$data) != 0){
      data_gr0 <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
      data_gr1 <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
      data_gr0 <- clean_specified_size(data_gr0, 2)
      data_gr1 <- clean_specified_size(data_gr1, 2)
      if(nrow(data_gr0) == 2){
        drawline0_done_cmb_bestfit$data <- TRUE
      }
      if(nrow(data_gr1) == 2){
        drawline1_done_cmb_bestfit$data <- TRUE
      }
      plot_module3_bestfit_cmb(group0 = TRUE, group1 = TRUE,data_gr0 = data_gr0, data_gr1 = data_gr1, data_0 = data_0, data_1 = data_1)
    }else{
      plot_module3_bestfit_cmb(group0 = FALSE, group1 = FALSE, data_0 = data_0, data_1 = data_1)
    }})
  
  
  which_graph_assess_line_done0_cmb_bestfit <- reactive({
    data_true <- data.frame(d = d_cmb_bestfit, x = x_cmb_bestfit, y = y_cmb_bestfit(), y_sample = y_cmb_bestfit() + residual_cmb_bestfit())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    data_current <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
    data_current <- clean_specified_size(data_current, 2)
    plot_module3_bestfit_cmb(group0 = TRUE, group1 = FALSE,data_current = data_current, data_0 = data_0, data_1 = data_1, done0 = TRUE)
  })
  
  which_graph_assess_line_done1_cmb_bestfit <- reactive({
    data_true <- data.frame(d = d_cmb_bestfit, x = x_cmb_bestfit, y = y_cmb_bestfit(), y_sample = y_cmb_bestfit() + residual_cmb_bestfit())
    data_0 <- subset(data_true, d == 0)
    data_1 <- subset(data_true, d == 1)
    data_current <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
    data_current <- clean_specified_size(data_current, 2)
    plot_module3_bestfit_cmb(group0 = FALSE, group1 = TRUE,data_current = data_current, data_0 = data_0, data_1 = data_1, done1 = TRUE)
  })
  
  which_graph_assess_line_done0half1_cmb_bestfit <- reactive({
    data_true <- data.frame(d = d_cmb_bestfit, x = x_cmb_bestfit, y = y_cmb_bestfit(), y_sample = y_cmb_bestfit() + residual_cmb_bestfit())
    data_gr0 <- subset(data_true, d == 0)
    data_gr1 <- subset(data_true, d == 1)
    data_0 <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
    data_1 <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- clean_specified_size(data_1, 2)
    plot_module3_bestfit_cmb(group0 = TRUE, group1 = TRUE,data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, done0 = TRUE)
  })
  
  which_graph_assess_line_done1half0_cmb_bestfit <- reactive({
    data_true <- data.frame(d = d_cmb_bestfit, x = x_cmb_bestfit, y = y_cmb_bestfit(), y_sample = y_cmb_bestfit() + residual_cmb_bestfit())
    data_gr0 <- subset(data_true, d == 0)
    data_gr1 <- subset(data_true, d == 1)
    data_0 <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
    data_1 <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- clean_specified_size(data_1, 2)
    plot_module3_bestfit_cmb(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, done1 = TRUE)
  })
  
  which_graph_assess_line_bothdone_cmb_bestfit <- reactive({
    formula <- paste0("y = ", coefficient_assess_guess_cmb_bestfit$b0, " + ", coefficient_assess_guess_cmb_bestfit$b1, "x + ", coefficient_assess_guess_cmb_bestfit$b2, "d + ", coefficient_assess_guess_cmb_bestfit$b3, "x*d + e, e ~ N(0, ", residualsd_assess_guess_cmb_bestfit$data, "^2)")
    data_true <- data.frame(d = d_cmb_bestfit, x = x_cmb_bestfit, y = y_cmb_bestfit(), y_sample = y_cmb_bestfit() + residual_cmb_bestfit())
    data_gr0 <- subset(data_true, d == 0)
    data_gr1 <- subset(data_true, d == 1)
    data_0 <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
    data_1 <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
    data_0 <- clean_specified_size(data_0, 2)
    data_1 <- clean_specified_size(data_1, 2)
    slope0 <- (data_0$y[2]-data_0$y[1])/(data_0$x[2]-data_0$x[1])
    slope1 <- (data_1$y[2]-data_1$y[1])/(data_1$x[2]-data_1$x[1])
    
    table <- create_table(c("b0", "b1", "b2", "b3"), c(coefficient_assess_guess_cmb_bestfit$b0, coefficient_assess_guess_cmb_bestfit$b1, coefficient_assess_guess_cmb_bestfit$b2, coefficient_assess_guess_cmb_bestfit$b3), 
                        c(round(data_0$y[1] - slope0*data_0$x[1],2), round(slope0,2), round(data_1$y[1] - slope1*data_1$x[1] - (data_0$y[1] - slope0*data_0$x[1]),2), round(slope1-slope0,2)), coef = TRUE)
    output$assessment_result_cmb_bestfit <- renderTable(table)
    plot_module3_bestfit_cmb(group0 = TRUE, group1 = TRUE, data_0 = data_0, data_1 = data_1, data_gr0 = data_gr0, data_gr1 = data_gr1, 
                             done0 = TRUE, done1 = TRUE, b0 = coefficient_assess_guess_cmb_bestfit$b0, b1 = coefficient_assess_guess_cmb_bestfit$b1, b2 = coefficient_assess_guess_cmb_bestfit$b2, b3 = coefficient_assess_guess_cmb_bestfit$b3, formula = formula)
  })
  
  output$plot_assess_draw_cmb_bestfit <- renderPlot({
    if ( drawline0_done_cmb_bestfit$data == TRUE & drawline1_done_cmb_bestfit$data == FALSE) {
      if(length(drawline_gr1_cmb_bestfit$data) == 0){
        which_graph_assess_line_done0_cmb_bestfit()
      }else{
        data_1 <- clean_to_dataframe(drawline_gr1_cmb_bestfit$data)
        data_1 <- clean_specified_size(data_1, 2)
        if(nrow(data_1) < 2){
          which_graph_assess_line_done0half1_cmb_bestfit()
        }else{
          drawline1_done_cmb_bestfit$data <- TRUE
        }
      }
    }else if(drawline0_done_cmb_bestfit$data == FALSE & drawline1_done_cmb_bestfit$data == TRUE){
      if(length(drawline_gr0_cmb_bestfit$data) == 0){
        which_graph_assess_line_done1_cmb_bestfit()
      }else{
        data_0 <- clean_to_dataframe(drawline_gr0_cmb_bestfit$data)
        data_0 <- clean_specified_size(data_0, 2)
        if(nrow(data_0) < 2){
          which_graph_assess_line_done1half0_cmb_bestfit()
        }else{
          drawline0_done_cmb_bestfit$data <- TRUE
        }
      }
    }else if(drawline0_done_cmb_bestfit$data == TRUE & drawline1_done_cmb_bestfit$data == TRUE){
      which_graph_assess_line_bothdone_cmb_bestfit()
    }else{
      which_graph_assess_line_cmb_bestfit()
    }
  })
  
  observeEvent(input$reset_assess_cmb_bestfit, {
    coefficient_assess_guess_cmb_bestfit$b0 <- sample(c(1:3,7:9), 1)
    if(coefficient_assess_guess_cmb_bestfit$b0 >= 5){
      coefficient_assess_guess_cmb_bestfit$b1 <- sample(seq(-2, -0.5, by = 0.5),1)
      coefficient_assess_guess_cmb_bestfit$b2 <- sample(c(-0.5:-2.5),1)
    }else{
      coefficient_assess_guess_cmb_bestfit$b1 <- sample(seq(0.5, 2, by = 0.5),1)
      coefficient_assess_guess_cmb_bestfit$b2 <- sample(c(0.5:2.5),1)
    }
    coefficient_assess_guess_cmb_bestfit$b3 <- sample(seq(0.5, 2, by = 0.5),1)
    residualsd_assess_guess_cmb_bestfit$data <- sample(seq(0.4,2.4, by = 0.2), 1)
    drawline_gr0_cmb_bestfit$data <- c()
    drawline_gr1_cmb_bestfit$data <- c()
    output$assessment_result_cmb_bestfit <- renderTable(data.frame())
    drawline0_done_cmb_bestfit$data <- FALSE
    drawline1_done_cmb_bestfit$data <- FALSE
  })
  
  observeEvent(input$plot_assess_click_cmb_bestfit, {
    pt <- input$plot_assess_click_cmb_bestfit
    if(input$which_group_cmb_bestfit == "group_0_cmb_bestfit"){
      drawline_gr0_cmb_bestfit$data <- c(drawline_gr0_cmb_bestfit$data, pt$x, pt$y)
    }else if(input$which_group_cmb_bestfit == "group_1_cmb_bestfit"){
      drawline_gr1_cmb_bestfit$data <- c(drawline_gr1_cmb_bestfit$data, pt$x, pt$y)
    }
  })
}