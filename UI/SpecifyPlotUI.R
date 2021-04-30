

SpecifyPlotUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel("Specifying Plot",
           tabsetPanel(
             tabPanel("Introdution", tags$div(
               style="margin: 50px 50px;",
               p("In this module, you will generate your own data set for regression by drawing points in a blank plot area. 
                             You will learn how the regression results change every time you draw a new data point. 
                             The sample size, residual standard deviation and coefficients are provided at the bottom."))),
             tabPanel("Fitting Models", fluidPage(
               dropdown(
                 
                 # tags$h4("Choose Type of Model"),
                 
                 pickerInput(inputId = ns('specify_model_type'),
                             label = "Choose Type of Model",
                             choices = c("One continuous predictor: y = b0 + b1x + eps" = "continuous_specify", 
                                         "One binary predictor: y = b0 + b1D + eps" = "binary_specify", 
                                         "One continuous predictor and one binary predictor (no interaction):
                                           y = a + b1*x + b2*D + eps" = "continuous+binary_specify",
                                         "One continuous predictor, one binary predictor and their interaction:
                                           y = a + b1*x + b2*D + b3*(x*D) + eps" = "continuous*binary_specify"),
                             options = list(`style` = "btn-info")),
                 style = "unite", icon = icon("gear"),
                 status = "danger", width = "300px",
                 animate = animateOptions(
                   enter = animations$fading_entrances$fadeInLeftBig,
                   exit = animations$fading_exits$fadeOutRightBig
                 )
               ),
               
               fluidRow(column(width = 11,
                               conditionalPanel("input.specify_model_type === 'continuous_specify'", ns = ns,                  
                                                plotOutput(ns("plot_specified"), height = 500,
                                                           click = ns("plot_specified_click")
                                                )),
                               conditionalPanel("input.specify_model_type === 'binary_specify'",  ns = ns,                  
                                                plotOutput(ns("plot_specified_binary"), height = 500,
                                                           click = ns("plot_specified_click_binary")
                                                )),
                               conditionalPanel("input.specify_model_type === 'continuous+binary_specify'",  ns = ns,                  
                                                plotOutput(ns("plot_specified_continuous_p_binary"), height = 500,
                                                           click = ns("plot_specified_click_continuous_p_binary")
                                                )),
                               conditionalPanel("input.specify_model_type === 'continuous*binary_specify'",  ns = ns,                  
                                                plotOutput(ns("plot_specified_continuous_m_binary"), height = 500,
                                                           click = ns("plot_specified_click_continuous_m_binary")
                                                ))
               )
               ),
               
               fluidRow(
                 column(width = 6,
                        conditionalPanel("input.specify_model_type === 'continuous_specify'", ns = ns,  
                                         selectInput(inputId = ns("draw_type"),
                                                     label = "Choose the type you want to draw:",
                                                     choices = c("Draw a line" = "draw_line", 
                                                                 "Draw points" = "draw_points"))),
                        conditionalPanel("input.specify_model_type === 'continuous_specify' && input.draw_type === 'draw_line'", ns = ns,  
                                         p("Click two points on the plot area to create a regression line and get a random sample that fit the regression line with your specified sample size and residual standard deviation. ")),
                        conditionalPanel("input.specify_model_type === 'continuous_specify' && input.draw_type === 'draw_points'", ns = ns,  
                                         p("Click on the plot area to create points and get a fitted regression line. ")),
                        conditionalPanel("input.specify_model_type === 'continuous_specify'", ns = ns,  
                                         p("You can use the Reset button to return to the original state of a blank panel."),
                                         actionButton(inputId = ns("reset"), "Reset")),
                        conditionalPanel("input.specify_model_type === 'binary_specify'", ns = ns,  
                                         p("Click points for gorup 0 at x = 0 and for group 1 at x = 1 within the shaded areas. Click 'Submit your points' when you are done with creating observations for the two groups."),
                                         p("You can use the Reset button to return to the original state of a blank panel."),
                                         actionButton(inputId = ns("submit_binary"), "Submit your points"),
                                         actionButton(inputId = ns("reset_binary_model"), "Reset")),
                        conditionalPanel("input.specify_model_type === 'continuous+binary_specify'", ns = ns,  
                                         p("Select which group you want to create observations for, and then click on the plot area to create points for the group. 
                           Click 'Submit your points' when you are done with creating observations for one group and setting the mean difference between the two groups."),
                                         p("You can use the Reset button to return to the original state of a blank panel."),
                                         selectInput(inputId = ns("which_group"),
                                                     label = "Choose Which Group to Create Points",
                                                     choices = c("Group 0" = "group_0", 
                                                                 "Group 1" = "group_1")),
                                         conditionalPanel("input.which_group === 'group_0'", ns = ns,  numericInput(inputId = ns("diff_gr1_gr0"), "The mean difference gorup 1 from group 0 (b2):", 3)),
                                         conditionalPanel("input.which_group === 'group_1'", ns = ns,  numericInput(inputId = ns("diff_gr0_gr1"), "The mean difference gorup 0 from group 1 (b2):", 3)),
                                         actionButton(inputId = ns("submit_continuous_p_binary"), "Submit your points"),
                                         actionButton(inputId = ns("reset_continuous_p_binary_model"), "Reset")),
                        conditionalPanel("input.specify_model_type === 'continuous*binary_specify'", ns = ns,  
                                         p("Select which group you want to create observations for, and then click on the plot area to create points for the group. 
                           You need to create points for both groups. Click 'Submit your points' when you are done with creating observations for both groups."),
                                         p("You can use the Reset button to return to the original state of a blank panel."),
                                         selectInput(inputId = ns("which_group_interaction"),
                                                     label = "Choose Which Group to Create Points",
                                                     choices = c("Group 0" = "group_0_interaction", 
                                                                 "Group 1" = "group_1_interaction")),
                                         actionButton(inputId = ns("submit_continuous_m_binary"), "Submit your points"),
                                         actionButton(inputId = ns("reset_continuous_m_binary_model"), "Reset"))),
                 
                 column(width = 6,
                        conditionalPanel("input.specify_model_type === 'continuous_specify' && input.draw_type === 'draw_points'", ns = ns,  tableOutput(ns("lm_results"))),
                        conditionalPanel("input.specify_model_type === 'continuous_specify' && input.draw_type === 'draw_line'", ns = ns,  
                                         sliderInput(inputId = ns("sample_size_draw"),
                                                     label = "Select Sample Size",
                                                     min = 10, max = 1000, value = 250, step = 10),
                                         sliderInput(inputId = ns("epsilon_error_draw"),
                                                     label = "Select Residual Std Dev (sigma)",
                                                     min = 0, max = 5, value = 0.3, step = 0.1),
                                         tableOutput(ns("draw_equation")), br()),
                        conditionalPanel("input.specify_model_type === 'binary_specify'", ns = ns,  
                                         textOutput(ns("binary_mean")), br(),
                                         textOutput(ns("binary_model")), tableOutput(ns("binary_equation"))),
                        conditionalPanel("input.specify_model_type === 'continuous+binary_specify'", ns = ns,  textOutput(ns("result_continuous_binary")), 
                                         tableOutput(ns("continuous_binary_equation"))),
                        conditionalPanel("input.specify_model_type === 'continuous*binary_specify'", ns = ns,  textOutput(ns("result_continuous_m_binary")), 
                                         tableOutput(ns("continuous_m_binary_equation"))))
               ),
               fluidRow(
                 column(width = 6),
                 column(width = 6, 
                        conditionalPanel("input.specify_model_type === 'continuous_specify' && input.draw_type === 'draw_points'", ns = ns,  
                                         textOutput(ns("n")),
                                         textOutput(ns("sigma"))))
               )
             ))))
}