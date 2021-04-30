# Module Server

server <- function(input, output){
  
  ############ tabPanel1 - "Specifying Coefficients" ############

  callModule(SpecifyCoef, "moduleone")

  ############### tabPanel2 - "Specifying Plot" #################
  
  callModule(SpecifyPlot, "moduletwo")
  
  ################ tabPanel3 - "Drawing Plot" ####################
  
  callModule(DrawPlot, "modulethree", input$inputName)
  
  ################### tabPanel4 - "Guessing" #####################

  callModule(GuessCoef, "modulefour")
}


