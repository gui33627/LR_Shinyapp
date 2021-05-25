

DrawPlotUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel("Drawing Plot", 
           tabsetPanel(
             tabPanel("Introdution", tags$div(
               style="margin: 50px 50px;",
               p("In this module, you will be assessed on how well you draw a regression line(s) or observations in consistent with the provided coefficients, or residual standard deviation and  
                             sample size, and on how well you draw the best fit line(s) given observations."))),
             tabPanel("Assessment", fluidPage(
               dropdown(
                 
                 pickerInput(inputId = ns('specify_model_type_draw'),
                             label = 'Choose Type of Model',
                             choices = c("One continuous predictor: y = b0 + b1x + eps" = "continuous_specify_draw", 
                                         "One binary predictor: y = b0 + b1D + eps" = "binary_specify_draw", 
                                         "One continuous predictor and one binary predictor (no interaction):
                                           y = a + b1*x + b2*D + eps" = "continuous+binary_specify_draw",
                                         "One continuous predictor, one binary predictor and their interaction:
                                           y = a + b1*x + b2*D + b3*(x*D) + eps" = "continuous*binary_specify_draw"),
                             options = list(`style` = "btn-info")),
                 pickerInput(inputId = ns('specify_test_mode'),
                             label = 'Choose Testing Mode',
                             choices = c("Coefficients and Residual Std.Dev." = "coef_stddev", 
                                         "Best Fit" = "best_fit"),
                             selected = "Coefficients and Residual Std.Dev.",
                             options = list(`style` = "btn-warning")),
                 style = "unite", icon = icon("gear"),
                 status = "danger", width = "300px",
                 animate = animateOptions(
                   enter = animations$fading_entrances$fadeInLeftBig,
                   exit = animations$fading_exits$fadeOutRightBig
                 )
               ),
               fluidRow(column(width = 6,
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                selectInput(inputId = ns("draw_type_assess"),
                                                            label = "Choose the type you want to draw:",
                                                            choices = c("Draw a line" = "draw_line_assess", 
                                                                        "Draw points" = "draw_points_assess"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess === 'draw_line_assess'", ns = ns,
                                                p("Based on the coefficients provided below, click two points in the plot area to determine a line that you think is best describing the regression line.
                          You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess === 'draw_points_assess'", ns = ns,
                                                p("Based on the coefficients, residual standard deviation and sample size provided below, 
                        click on the plot area the specified number of points that you think is most possible to fit the model of one continuous predictor with the provided coefficients and residual standard deviation.
                        You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                textOutput(ns("intercept_assess_draw")), textOutput(ns("coefficient_assess_draw"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess === 'draw_points_assess'", ns = ns,
                                                textOutput(ns("residualsd_assess_draw")), textOutput(ns("samplesize_assess_draw"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns, actionButton(inputId = ns("reset_assess"), "Next one!")),
                               
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'best_fit'", ns = ns,
                                                p("Based on the points provided below, click two points in the plot area to determine a line that you think is best fit regression line.
                          You can use the 'Next one!' button to go to the next question."), actionButton(inputId = ns("reset_assess_bestfit"), "Next one!")),
                               
                               
                               
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                selectInput(inputId = ns("draw_type_assess_binary"),
                                                            label = "Choose the type you want to draw:",
                                                            choices = c("Draw mean points" = "draw_line_assess_binary", 
                                                                        "Draw obs. points" = "draw_points_assess_binary"))),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_binary === 'draw_line_assess_binary'", ns = ns,
                                                p("Based on the coefficients provided below, click two points in the plot area that you think is best describing the means of the two groups.
                          You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_binary === 'draw_points_assess_binary'", ns = ns,
                                                p("Based on the coefficients, residual standard deviation and sample size provided below, 
                        click on the plot area the specified number of points that you think is most possible to fit the model of one binary predictor with the provided coefficients and residual standard deviation.
                        You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                textOutput(ns("intercept_assess_draw_binary")), textOutput(ns("coefficient_assess_draw_binary"))),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_binary === 'draw_points_assess_binary'", ns = ns,
                                                textOutput(ns("residualsd_assess_draw_binary")), textOutput(ns("samplesize_assess_draw_binary"))),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns, actionButton(inputId = ns("reset_assess_binary"), "Next one!")),
                               
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'best_fit'", ns = ns,
                                                p("Based on the points provided below, click two points in the plot area that you think is best describing the means of the two groups.
                          You can use the 'Next one!' button to go to the next question."), actionButton(inputId = ns("reset_assess_binary_bestfit"), "Next one!")),
                               
                               
                               
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                selectInput(inputId = ns("draw_type_assess_continuous_p_binary"),
                                                            label = "Choose the type you want to draw:",
                                                            choices = c("Draw lines" = "draw_line_assess_continuous_p_binary", 
                                                                        "Draw points" = "draw_points_assess_continuous_p_binary"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_p_binary === 'draw_line_assess_continuous_p_binary'", ns = ns,
                                                p("Based on the coefficients provided below, click two points for each group in the plot area to determine two lines that you think is best describing the regression lines.
                          You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_p_binary === 'draw_points_assess_continuous_p_binary'", ns = ns,
                                                p("Based on the coefficients, residual standard deviation and sample size provided below, 
                        click on the plot area the specified number of points for one group and move the points for the other group that you think is most consistent with the parameters.
                        You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                textOutput(ns("intercept_assess_draw_cpb")), textOutput(ns("coefficient_assess_draw_cpb_b1")), textOutput(ns("coefficient_assess_draw_cpb_b2"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_p_binary === 'draw_points_assess_continuous_p_binary'", ns = ns,
                                                textOutput(ns("residualsd_assess_draw_cpb")), textOutput(ns("samplesize_assess_draw_cpb"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                selectInput(inputId = ns("which_group_cpb"),
                                                            label = "Choose Which Group to Create Line",
                                                            choices = c("Group 0" = "group_0_cpb", 
                                                                        "Group 1" = "group_1_cpb"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_p_binary === 'draw_points_assess_continuous_p_binary'", ns = ns,
                                                p("Please input a value or click on up/down arrow to put the points for the other group in the place you think consistent with the parameters provided."),
                                                numericInput(inputId = ns("move_cpb"), "The distance to move the points up/down for the second group:", value = 1, step = 0.5),
                                                actionButton(inputId = ns("submit_moved_points"),"Submit points") ),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_p_binary === 'draw_line_assess_continuous_p_binary'", ns = ns,
                                                numericInput(inputId = ns("move_cpb_line"), "The distance to move the line up/down for the second group:", value = 1, step = 0.5),
                                                actionButton(inputId = ns("submit_drag_coef"), "Submit lines")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                actionButton(inputId = ns("reset_assess_cpb"), "Next one!")),
                               
                               
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'best_fit'", ns = ns,
                                                p("Based on the points provided below, click two points to determine a line for one group in the plot area that you think is best describing its regression line. Then drag the line for the second group to the position that you think is appropriate and submit your lines by 'Submit lines' button.
                          You can use the 'Next one!' button to go to the next question."),
                                                selectInput(inputId = ns("which_group_cpb_bestfit"), label = "Choose Which Group to Create Line", 
                                                            choices = c("Group 0" = "group_0_cpb_bestfit", "Group 1" = "group_1_cpb_bestfit")),
                                                numericInput(inputId = ns("move_cpb_line_bestfit"), "The distance to move the line up/down for the second group:", value = 1, step = 0.5),
                                                actionButton(inputId = ns("submit_drag"), "Submit lines"), actionButton(inputId = ns("reset_assess_cpb_bestfit"), "Next one!")),
                               
                               
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                selectInput(inputId = ns("draw_type_assess_continuous_m_binary"),
                                                            label = "Choose the type you want to draw:",
                                                            choices = c("Draw lines" = "draw_line_assess_continuous_m_binary", 
                                                                        "Draw points" = "draw_points_assess_continuous_m_binary"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_m_binary === 'draw_line_assess_continuous_m_binary'", ns = ns,
                                                p("Based on the coefficients provided below, click two points for each group in the plot area to determine two lines that you think is best describing the regression lines.
                          You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_m_binary === 'draw_points_assess_continuous_m_binary'", ns = ns,
                                                p("Based on the coefficients, residual standard deviation and sample size provided below, 
                        click on the plot area the specified number of points that you think is most possible to fit the model of one continuous predictor and one binary predictor and their interaction with the provided coefficients and residual standard deviation.
                        You can use the 'Next one!' button to go to the next question.")),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                textOutput(ns("intercept_assess_draw_cmb")), textOutput(ns("coefficient_assess_draw_cmb_b1")), textOutput(ns("coefficient_assess_draw_cmb_b2")), textOutput(ns("coefficient_assess_draw_cmb_b3"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_m_binary === 'draw_points_assess_continuous_m_binary'", ns = ns,
                                                textOutput(ns("residualsd_assess_draw_cmb")), textOutput(ns("samplesize_assess_draw_cmb"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", ns = ns,
                                                selectInput(inputId = ns("which_group_cmb"),
                                                            label = "Choose Which Group to Create Line",
                                                            choices = c("Group 0" = "group_0_cmb", 
                                                                        "Group 1" = "group_1_cmb")),
                                                actionButton(inputId = ns("reset_assess_cmb"), "Next one!")),
                               
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'best_fit'", ns = ns,
                                                p("Based on the points provided below, click two points for each group in the plot area to determine two lines that you think is best describing the regression lines.
                          You can use the 'Next one!' button to go to the next question."),
                                                selectInput(inputId = ns("which_group_cmb_bestfit"),
                                                            label = "Choose Which Group to Create Line",
                                                            choices = c("Group 0" = "group_0_cmb_bestfit", 
                                                                        "Group 1" = "group_1_cmb_bestfit")),
                                                actionButton(inputId = ns("reset_assess_cmb_bestfit"), "Next one!")),
               ),
               column(width = 6,
                      p("Your assessment result:"),
                      conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                       ns = ns, tableOutput(ns("assessment_result"))),
                      conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                       ns = ns, textOutput(ns("assessment_result_binary_text")),tableOutput(ns("assessment_result_binary"))),
                      conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                       ns = ns, textOutput(ns("assessment_result_cpb_text")),tableOutput(ns("assessment_result_cpb"))),
                      conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                       ns = ns, tableOutput(ns("assessment_result_cmb"))),
                      
                      conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                       ns = ns, tableOutput(ns("assessment_result_bestfit"))),
                      conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                       ns = ns, tableOutput(ns("assessment_result_binary_bestfit"))),
                      conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                       ns = ns, tableOutput(ns("drag")), textOutput(ns("assessment_result_cpb_bestfit"))),
                      conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                       ns = ns, tableOutput(ns("assessment_result_cmb_bestfit"))),
                      
                      conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess === 'draw_line_assess'", 
                                       ns = ns, tableOutput(ns("data_info"))),
                      conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev' && input.draw_type_assess_continuous_p_binary === 'draw_line_assess_continuous_p_binary'", 
                                       ns = ns, tableOutput(ns("data_info_assess"))))),
               fluidRow(column(width = 11,
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw"), height = 500, click = ns("plot_assess_click"))),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_binary"), height = 500, click = ns("plot_assess_click_binary"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_cpb"), height = 500, click = ns("plot_assess_click_cpb"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'coef_stddev'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_cmb"), height = 500, click = ns("plot_assess_click_cmb"))),
                               
                               conditionalPanel("input.specify_model_type_draw === 'continuous_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_bestfit"), height = 500, click = ns("plot_assess_click_bestfit"))),
                               conditionalPanel("input.specify_model_type_draw === 'binary_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_binary_bestfit"), height = 500, click = ns("plot_assess_click_binary_bestfit"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous+binary_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_cpb_bestfit"), height = 500, click = ns("plot_assess_click_cpb_bestfit"))),
                               conditionalPanel("input.specify_model_type_draw === 'continuous*binary_specify_draw' && input.specify_test_mode === 'best_fit'", 
                                                ns = ns, plotOutput(ns("plot_assess_draw_cmb_bestfit"), height = 500, click = ns("plot_assess_click_cmb_bestfit"))),
               ))))))
}