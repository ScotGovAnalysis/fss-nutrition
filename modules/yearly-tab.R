

# UI ----------------------------------------------------------------------

yearlyTabUI <- function(id){
  
  ns <- NS(id)
  
  fluidRow(
    
    
  ),
  fluidRow(
    column(
      width = 2,
      bsButton(ns("metric1"),
               label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric1_text")),"</b><p style='font-size:14px'>Metric text</p>")),
               style = "overall",
               size = "small",
               width = "100%"
      )
    ),
    column(width = 2,
           bsButton(ns("metric2"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric2_text")),"</b><p style='font-size:14px'>Metric text<p style='font-size:14px'>")),
                    style = "chem",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric3"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric3_text")),"</b><p style='font-size:14px'>Metric text")),
                    style = "micro",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric4"),
                    label = HTML(paste0("<b style='font-size:20px'>", textOutput(ns("metric4_text")),"</b><p style='font-size:14px'>Metric text")),
                    #icon = icon("thumbs-o-up"),
                    style = "authenticity",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric5"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric5_text")),"</b><p style='font-size:14px'>Metric text")),
                    #icon = icon("tag"),
                    style = "labelling",
                    size = "small",
                    width = "100%")
           
           
    ),
    column(width = 2,
           bsButton(ns("metric6"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric6_text")),"</b><p style='font-size:14px'>Metric text")),
                    #icon = icon("tag"),
                    style = "allergen",
                    size = "small",
                    width = "100%")
           
           
    ),
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




