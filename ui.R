
ui <- fluidPage(
  
  titlePanel("Linear Regression"),
  navlistPanel(widths = c(2, 10),
               "Learning",
               SpecifyCoefUI("moduleone"),
               SpecifyPlotUI("moduletwo"),
               "Testing",
               DrawPlotUI("modulethree"), 
               GuessCoefUI("modulefour")
  )
  
)

