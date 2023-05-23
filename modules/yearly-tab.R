

# UI ----------------------------------------------------------------------

yearlyTabUI <- function(id){
  
  ns <- NS(id)
  tagList(
  fluidRow(
    column(width = 12, pickerInput(ns("select_year"), 
                   label = "Select year of interest: ", 
                   choices = c(2019, 2020, 2021, 2022)))
    
  ),
  fluidRow(
    column(
      width = 2,
      bsButton(ns("metric1"),
               label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric1_text")),"</b><p style='font-size:14px'>Metric text</p>")),
               style = "metric",
               size = "small",
               width = "100%"
      )
    ),
    column(width = 2,
           bsButton(ns("metric2"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric2_text")),"</b><p style='font-size:14px'>Metric text<p style='font-size:14px'>")),
                    style = "metric",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric3"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric3_text")),"</b><p style='font-size:14px'>Metric text")),
                    style = "metric",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric4"),
                    label = HTML(paste0("<b style='font-size:20px'>", textOutput(ns("metric4_text")),"</b><p style='font-size:14px'>Metric text")),
                    style = "metric",
                  #  icon = icon("thumbs-o-up"),
                   size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric5"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric5_text")),"</b><p style='font-size:14px'>Metric text")),
                  #  icon = icon("tag"),
                    style = "metric",
                    size = "small",
                    width = "100%")
           
           
    ),
    column(width = 2,
           bsButton(ns("metric6"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric6_text")),"</b><p style='font-size:14px'>Metric text")),
                    #icon = icon("tag"),
                    style = "metric",
                    size = "small",
                    width = "100%")
           
           
    )
    ), 
  fluidRow(br()), 
  
  fluidRow(
    box(width = 4, plotOutput(ns("plot1"))),
    
    box(width = 4, plotOutput(ns("plot2"))),
    
    box(width = 4, plotOutput(ns("plot3")))
  )
  ) 
  
}


# server ------------------------------------------------------------------

navButtonsServer <- function(id, parent1 = session, sfsd_data, result_filter) {
  
  moduleServer(id,
               function(input, output, session, parent = parent1) {
                 

                 # Calculate sample counts to display in buttons
                 
                 output$metric1_text<- renderText({
                 })
                 
                 output$metric2_text <- renderText({
                 })
                 
                 output$metric3_text <- renderText({
                 })
                 
                 output$metric4_text <- renderText({
                 })
                 output$metric5_text <- renderText({
                 })
                 
                 output$metric6_text <- renderText({
                 })
                 
                 
               }
               
  )
}




