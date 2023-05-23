# This module provides the ui and server for everything on the overall tab.
# The UI takes in an id to link it to the server

overtimeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    box(width = 12, 
        fluidRow(
column(width = 12, 
             radioGroupButtons(ns("metric"), 
                                   label = "Select the metric to display: ", 
                                   choices = c("Spend", "Nutritional volume", "Energy (kcal)"))
    )),
    
    
 column(width = 12,    fluidRow(

             box(width = 4, plotOutput(ns("plot1"))),

             box(width = 4, plotOutput(ns("plot2"))),

             box(width = 4, plotOutput(ns("plot3")))
  ))),
  fluidRow(
    box(width = 4, plotOutput(ns("plot5"))),
    
    box(width = 4, plotOutput(ns("plot6"))),
    
    box(width = 4, plotOutput(ns("plot7")))
  ), 
  fluidRow(
    box(width = 4, plotOutput(ns("plot8"))),
    
    box(width = 4, plotOutput(ns("plot9"))),
    
    box(width = 4, plotOutput(ns("plot10")))
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