

GuessCoefUI <- function(id){
  
  ns <- NS(id)
  
  tabPanel("Guessing Coefficients", 
           tabsetPanel(
             tabPanel("Introdution", tags$div(
               style="margin: 50px 50px;",
               p("In this module, you will be assessed how well you guess the coefficients or residual standard deviation based on the provided line plot or points plot."))),
             tabPanel("Assessment", fluidPage(
               dropdown(
                 
                 tags$h4("Choose Type of Model"),
                 
                 pickerInput(inputId = ns('specify_model_type_guess'),
                             label = '',
                             choices = c("One continuous predictor: y = b0 + b1x + eps" = "continuous_specify_guess", 
                                         "One binary predictor: y = b0 + b1D + eps" = "binary_specify_guess", 
                                         "One continuous predictor and one binary predictor (no interaction):
                                           y = a + b1*x + b2*D + eps" = "continuous+binary_specify_guess",
                                         "One continuous predictor, one binary predictor and their interaction:
                                           y = a + b1*x + b2*D + b3*(x*D) + eps" = "continuous*binary_specify_guess"),
                             options = list(`style` = "btn-info")),
                 style = "unite", icon = icon("gear"),
                 status = "danger", width = "300px",
                 animate = animateOptions(
                   enter = animations$fading_entrances$fadeInLeftBig,
                   exit = animations$fading_exits$fadeOutRightBig
                 )
               ),
               fluidRow(column(width = 6,
                               conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess'", ns = ns, 
                                                selectInput(inputId = ns("guess_type_assess"),
                                                            label = "Choose the type you want to guess:",
                                                            choices = c("Guess coefficients" = "guess_coef", 
                                                                        "Guess Residual Std. Dev.(Sigma)" = "guess_residual"))),
                               conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess'", ns = ns, 
                                                selectInput(inputId = ns("guess_type_assess_binary"),
                                                            label = "Choose the type you want to guess:",
                                                            choices = c("Guess coefficients" = "guess_coef_binary", 
                                                                        "Guess Residual Std. Dev.(Sigma)" = "guess_residual_binary"))),
                               conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess'", ns = ns, 
                                                selectInput(inputId = ns("guess_type_assess_cpb"),
                                                            label = "Choose the type you want to guess:",
                                                            choices = c("Guess coefficients" = "guess_coef_cpb", 
                                                                        "Guess Residual Std. Dev.(Sigma)" = "guess_residual_cpb"))),
                               conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess'", ns = ns, 
                                                selectInput(inputId = ns("guess_type_assess_cmb"),
                                                            label = "Choose the type you want to guess:",
                                                            choices = c("Guess coefficients" = "guess_coef_cmb", 
                                                                        "Guess Residual Std. Dev.(Sigma)" = "guess_residual_cmb"))),
                               p("You can use the 'Next one!' button to go to the next guess question."),
                               conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess'", ns = ns, actionButton(inputId = ns("reset_guess"), "Next one!")),
                               conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess'", ns = ns, actionButton(inputId = ns("reset_guess_binary"), "Next one!")),
                               conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess'", ns = ns, actionButton(inputId = ns("reset_guess_cpb"), "Next one!")),
                               conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess'", ns = ns, actionButton(inputId = ns("reset_guess_cmb"), "Next one!")),
               ),
               column(width = 6,
                      conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess' && input.guess_type_assess === 'guess_coef'", ns = ns,      
                                       p("Fill in the coefficients (b0 and b1) based on your observing the plot below. "),
                                       numericInput(inputId = ns("guess_b0"), "Intercept (b0):", 0.5),
                                       numericInput(inputId = ns("guess_b1"), "Coefficient on x (b1):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess' && input.guess_type_assess === 'guess_residual'", ns = ns,  
                                       p("Fill in the residual standard deviation (sigma) based on your observing the plot below. "), 
                                       numericInput(inputId = ns("guess_sigma"), "Residual Std.Dev.(sigma):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess'", ns = ns, actionButton(inputId = ns("submit"), "Submit")),
                      
                      conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess' && input.guess_type_assess_binary === 'guess_coef_binary'", ns = ns,      
                                       p("Fill in the coefficients (b0 and b1) based on your observing the plot below. "),
                                       numericInput(inputId = ns("guess_b0_binary"), "Intercept (b0):", 0.5),
                                       numericInput(inputId = ns("guess_b1_binary"), "Coefficient on d (b1):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess' && input.guess_type_assess_binary === 'guess_residual_binary'", ns = ns, 
                                       p("Fill in the residual standard deviation (sigma) based on your observing the plot below. "), 
                                       numericInput(inputId = ns("guess_sigma_binary"), "Residual Std.Dev.(sigma):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess'", ns = ns, actionButton(inputId = ns("submit_guess_binary"), "Submit")),
                      
                      conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess' && input.guess_type_assess_cpb === 'guess_coef_cpb'", ns = ns,      
                                       p("Fill in the coefficients (b0, b1 and b2) based on your observing the plot below. "),
                                       numericInput(inputId = ns("guess_b0_cpb"), "Intercept (b0):", 0.5),
                                       numericInput(inputId = ns("guess_b1_cpb"), "Coefficient on x (b1):", 0.5),
                                       numericInput(inputId = ns("guess_b2_cpb"), "Coefficient on d (b2):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess' && input.guess_type_assess_cpb === 'guess_residual_cpb'", ns = ns,   
                                       p("Fill in the residual standard deviation (sigma) based on your observing the plot below. "), 
                                       numericInput(inputId = ns("guess_sigma_cpb"), "Residual Std.Dev.(sigma):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess'", ns = ns, actionButton(inputId = ns("submit_cpb"), "Submit")),
                      
                      conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess' && input.guess_type_assess_cmb === 'guess_coef_cmb'", ns = ns,      
                                       p("Fill in the coefficients (b0, b1, b2 and b3) based on your observing the plot below. "),
                                       numericInput(inputId = ns("guess_b0_cmb"), "Intercept (b0):", 0.5),
                                       numericInput(inputId = ns("guess_b1_cmb"), "Coefficient on x (b1):", 0.5),
                                       numericInput(inputId = ns("guess_b2_cmb"), "Coefficient on d (b2):", 0.5),
                                       numericInput(inputId = ns("guess_b3_cmb"), "Coefficient on x*d (b3):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess' && input.guess_type_assess_cmb === 'guess_residual_cmb'", ns = ns,  
                                       p("Fill in the residual standard deviation (sigma) based on your observing the plot below. "), 
                                       numericInput(inputId = ns("guess_sigma_cmb"), "Residual Std.Dev.(sigma):", 0.5)),
                      conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess'", ns = ns, actionButton(inputId = ns("submit_cmb"), "Submit")),
                      
                      p("Your assessment result:"),
                      conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess'", ns = ns, tableOutput(ns("guess_result"))),
                      conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess'", ns = ns, tableOutput(ns("guess_result_binary"))),
                      conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess'", ns = ns, tableOutput(ns("guess_result_cpb"))),
                      conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess'", ns = ns, tableOutput(ns("guess_result_cmb")))
               )), 
               fluidRow(column(width = 11, 
                               conditionalPanel("input.specify_model_type_guess === 'continuous_specify_guess'", ns = ns, plotOutput(ns("plot_assess_fix"), height = 500)),
                               conditionalPanel("input.specify_model_type_guess === 'binary_specify_guess'", ns = ns, plotOutput(ns("plot_assess_fix_binary"), height = 500)),
                               conditionalPanel("input.specify_model_type_guess === 'continuous+binary_specify_guess'", ns = ns, plotOutput(ns("plot_assess_fix_cpb"), height = 500)),
                               conditionalPanel("input.specify_model_type_guess === 'continuous*binary_specify_guess'", ns = ns, plotOutput(ns("plot_assess_fix_cmb"), height = 500))))
             ))))
  
  
}