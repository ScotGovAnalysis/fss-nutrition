source("scripts/global.R")
ui <- tagList(  tags$head(
  tags$link(rel = "stylesheet", type = "text/css", href = "dashboard_style.css")),
  dashboardPage(
    title = "FSS Nutrition Dashboard",
    
    
    # HEADER ------------------------------------------------------------------
    dashboardHeader(
      title = div(tags$a(href='https://www.foodstandards.gov.scot/',
                         tags$img(src='fss_logo.png', width = 80)), "FSS Nutrition Dashboard"),
      titleWidth = 410
    ),
    
    
    # SIDEBAR  ---------------------------------------------------------------
    
    dashboardSidebar(
      sidebarMenu(menuItem("  Trends over time", tabName = "overtime", icon = icon("chart-line")),
                  menuItem("  Yearly profile", tabName = "yearly", icon = icon("bars-progress")),
                  menuItem("  F&D categories", tabName = "categories", icon = icon("carrot")),
                  menuItem("  About ", tabName = "about", icon = icon("info")))
      
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
      tabItems(
        
        tabItem(tabName = "overtime",
                overtimeTabUI("overtime")
                
        ), 
        
        tabItem(tabName = "yearly",
                yearlyTabUI("yearly")
                
        ), 
        
        tabItem(tabName = "categories",
                categoryTabUI("promo")
                
        ), 
        
        tabItem(tabName = "about",
              tagList(box(width = 12, title = "About this dashboard", HTML(paste(about_text, collapse = "<br>"))))
        )
        
        
        
        
      )         
    )
  )
)







server <- function(input, output, session) {
  
  shinyalert("Welcome", "Please do not share screenshots or downloads from this dashboard without prior permission from ...", type = "info")
  
  
  overtimeTabServer("overtime", overall, promotype, online, totals_pppd, category)
  
  
  yearlyTabServer("yearly", overall, promotype, online, totals_pppd)
  

  categoryTabServer("promo", category_promo_totals, category_simd)
  
  
  
}





# Run the application 
shinyApp(ui = ui, server = server)
