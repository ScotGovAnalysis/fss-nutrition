# This module provides the ui and server for everything on the overall tab.
# The UI takes in an id to link it to the server

overtimeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(width = 8,
             radioGroupButtons(ns("metric"), 
                                   label = "Select the metric to display: ", 
                                   choices = c("Spend", "Nutritional volume", "Energy (kcal)")))
    ),
    
    
    fluidRow(

             box(width = 4, plotOutput(ns("plot1"))),

             box(width = 4, plotOutput(ns("plot2"))),

             box(width = 4, plotOutput(ns("plot3")))
  ),
  fluidRow(
    box(width = 4, plotOutput(ns("plot5"))),
    
    box(width = 4, plotOutput(ns("plot6"))),
    
    box(width = 4, plotOutput(ns("plot7")))
  )
  )
}



# server ------------------------------------------------------------------
# the server takes in an id and the data. 
# it renders all of the plots and tables on the overall tab

overtimeTabServer <- function(id, overtime_data) {
  moduleServer(id, function(input, output, session) {
    

    

    
    
  })
}