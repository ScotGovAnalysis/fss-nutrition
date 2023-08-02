


# UI ----------------------------------------------------------------------

categoryTabUI <- function(id){
  
  ns <- NS(id)
  tagList(
    
    column(width = 9, 
           fluidRow(column(width = 6, 
                           pickerInput(ns("select_year"), 
                                       label = "Select year of interest: ", 
                                       
                                       choices = c("2019", "2020", "2021", "2022", "2019-2022"))),
                    column(width = 6, 
                           pickerInput(ns("xaxis"), 
                                       label = "Select a variable for the x-axis: ", 
                                       
                                       choices = c("Total nutritional volume purchased" = "NutritionalVolume", 
                                                   "Nutritional volume purchased as a percentage of total food and drink" = "pctg_total_fd", 
                                                   "Nutritional volume purchased on a price promotion (all retail)"  = "pctg_price_promo", 
                                                   "Nutritional volume by SIMD" = "simd")))),
           fluidRow(box(width = 12, plotlyOutput(ns("plot1"), height = 600)
           ))
    ),
    
    column(width = 3,
           
           checkboxGroupInput(
             inputId = ns("category"),
             label = "Select categories for the plot: ", 
             choices = unique(category$`F&D Category`)
           ), 
           checkboxInput(ns("bar"), 'Select/deselect all') 
           #   actionButton(ns("generate"), label = "Generate plot")
    )
    
    
    
    
    
  ) 
  
}


# server ------------------------------------------------------------------

categoryTabServer <- function(id, data_promo, data_simd) {
  
  moduleServer(id,
               function(input, output, session, parent = parent1) {
                 
                 observe({
                   updateCheckboxGroupInput(
                     session, 'category', choices = unique(category$`F&D Category`),
                     selected = if (input$bar) unique(category$`F&D Category`)
                   )
                 })
                 
                 # 
                 # data_for_plot <- eventReactive(input$generate, {
                 #   
                 #   data %>%
                 #     filter(Year == input$select_year) %>%
                 #     filter(`F&D Category` %in% input$category) 
                 #   
                 # })
                 
                 
                 
                 output$plot1 <- renderPlotly({
                   
                   if (input$xaxis == "simd"){
                     validate(
                       need(length(input$category)>0 & length(input$category) <= 8, "Select up to 8 categories from the list on the right to include in the plot")
                     )
                     
                     
                     
                     data_simd %>%
                       filter(!SIMD %in% c("Total Household", "No SIMD")) %>%
                       filter(Year == input$select_year) %>%
                       filter(`F&D Category` %in% input$category) %>%
                       #  mutate(`F&D Category` = fct_reorder(`F&D Category`, !!as.symbol("NutritionalVolume"))) %>%
                       #data_for_plot() %>%
                       ggplot() +
                       aes(x = `Nutritional Volume %`, y =`F&D Category`, fill = SIMD ) +
                       geom_col(position = "dodge") +
                       scale_fill_discrete_sg("main6") +
                       theme_classic()+
                       #  theme_sg() +
                       # theme(text = element_text(family = "")) +
                       labs(x = "% of total nutritional volume purchased in retail by SIMD group",
                            title = paste0("Nutritional volume of food and drink categories purchased as a percentage of total food and drink purchased by SIMD during ", input$select_year), 
                            y = " ")
                     
                     
                     
                   } else {
                     
                     validate(
                       need(length(input$category)>0, "Please select categories from the list on the right to include in the plot")
                     )
                     
                     plot_title <- if (input$xaxis == "NutritionalVolume") {paste0("Nutritional volume of food and drink purchased during ", input$select_year, " by category")
                     } else if (input$xaxis == "pctg_total_fd" ){paste0("Nutritional volume of food and drink categories purchased as a percentage of total retail during ", input$select_year)} else {
                       paste0("Nutritional volume of food and drink purchased on a price promotion during ", input$select_year, " by category")
                     }
                     
                     
                     data_promo %>%
                       filter(Year == input$select_year) %>%
                       filter(`F&D Category` %in% input$category) %>%
                       mutate(`F&D Category` = fct_reorder(`F&D Category`, !!as.symbol(input$xaxis))) %>%
                       #data_for_plot() %>%
                       ggplot() +
                       aes(x = !!as.symbol(input$xaxis), y =`F&D Category` ) +
                       geom_col(width = 0.8) +
                       theme_classic()+
                       #  theme_sg() +
                       # theme(text = element_text(family = "")) +
                       labs(x = ifelse(input$xaxis == "NutritionalVolume", "Nutritional volume", "Percentage of total (%)"),
                            title = plot_title, 
                            y = " ")
                     
                     
                   }
                   
                   
                   
                   
                   
                 })
                 
                 
                 
                 
                 
                 
                 
               }
  )
}
               
  



