library(shiny)
library(shinydashboard)
library(dashboardthemes)


source("scripts/dashboard_theme.R")



ui <- tagList(  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "dashboard_style.css")),
    dashboardPage(
        title = "FSS Dashboard Template",
        
        
        # HEADER ------------------------------------------------------------------
        dashboardHeader(
            title = div(tags$a(href='https://www.foodstandards.gov.scot/',
                               tags$img(src='fss_logo.png', width = 80)), "FSS Dashboard Template"),
            titleWidth = 410
        ),
        
        
        # SIDEBAR  ---------------------------------------------------------------
        
        dashboardSidebar(
            
        ),
        
        
        # BODY --------------------------------------------------------------------
        
        dashboardBody(
            # tags$head(
            #     tags$link(
            #         rel = "stylesheet", 
            #         type = "text/css", 
            #         href = "radar_style.css")
            # ),
            customTheme,

            
            ######## INSERT BODY ###############
            
          
          
)
)
)







server <- function(input, output, session) {
    
  
}



# Run the application 
shinyApp(ui = ui, server = server)
