
# This tab produces a range of plots. The user is able to chose a year (or view data from all 4 years combined), the variable for the x-axis as well as the categories of interest


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
                                                   "Total annual spend as a percentage of total food and drink" = "pctg_spend",  
                                                   "Nutritional volume purchased on a price promotion (all retail)"  = "pctg_price_promo", 
                                                   "Nutritional volume by SIMD" = "simd")))),
           fluidRow(box(width = 12, plotOutput(ns("plot1"), height = 600)
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
                 
                 # Select all button functionality 
                 observe({
                   updateCheckboxGroupInput(
                     session, 'category', choices = unique(category$`F&D Category`),
                     selected = if (input$bar) unique(category$`F&D Category`)
                   )
                 })
                 
                 
                 
                 
                 output$plot1 <- renderPlot({
                   


# SIMD plot ---------------------------------------------------------------

                 # For the SIMD plot there cannot be more than 8 categories selected
                   
                   if (input$xaxis == "simd"){
                     validate(
                       need(length(input$category)>0 & length(input$category) <= 8, "Select up to 8 categories from the list on the right to include in the plot")
                     )
                     
                    
                  # Data prep    
                     
                     data_simd %>%
                       filter(!SIMD %in% c("Total Household", "No SIMD")) %>%
                       filter(Year == input$select_year) %>%
                       filter(`F&D Category` %in% input$category) %>%
                       ggplot() +
                       aes(x = `Nutritional Volume %`, y =`F&D Category`, fill = SIMD ) +
                       geom_col(position = "dodge") +
                       scale_fill_discrete_sg("main6", palette_type = "af") +
                       theme_classic(base_size = 19)+
                       theme(plot.title.position = "plot", 
                             plot.title = element_text(size = 18)) +
                       labs(x = "% of total nutritional volume purchased in retail by SIMD group",
                            title = paste0("Nutritional volume of food and drink categories purchased as a percentage of total food and drink purchased by SIMD during ", input$select_year), 
                            y = "Category")
                     

# Remaining plots (not SIMD related) --------------------------------------

                     
                     
                   } else {
                     
                     validate(
                       need(length(input$category)>0, "Please select categories from the list on the right to include in the plot")
                     )
                     
                     # Dynamic plot titles depending on which xaxis var is selected 
                     
                     plot_title <- if (input$xaxis == "NutritionalVolume") {paste0("Nutritional volume of food and drink purchased during ", 
                                                                                   input$select_year, " by category")
                     } else if (input$xaxis == "pctg_total_fd"){paste0("Nutritional volume of food and drink categories purchased as a percentage of total retail during ", input$select_year)
                       } else if (input$xaxis == "pctg_spend") {
                         paste0("Annual spend of food and drink purchased on a price promotion during ", input$select_year, " by category")
                       }else{
                       paste0("Nutritional volume of food and drink purchased on a price promotion during ", input$select_year, " by category")
                     }
                     
                     # Plot 
                     
                     data_promo %>%
                       filter(Year == input$select_year) %>%
                       filter(`F&D Category` %in% input$category) %>%
                       mutate(`F&D Category` = fct_reorder(`F&D Category`, !!as.symbol(input$xaxis)), 
                              # bar highlight
                              col_fill = if_else(`F&D Category` == "Total Food & Drink", "high", "reg")) %>%
                       ggplot() +
                       aes(x = !!as.symbol(input$xaxis), y =`F&D Category`, fill = col_fill ) +
                       geom_col(width = 0.8) +
                       theme_classic()+
                       scale_fill_discrete_sg("focus")+
                       labs(x = ifelse(input$xaxis == "NutritionalVolume", "Nutritional volume", "Percentage of total (%)"),
                            title = plot_title, 
                            y = " ") +
                       theme(legend.position = "none")
                     
                     
                   }
                   
                   
                   
                   
                   
                 })
                 
                 
                 
                 
                 
                 
                 
               }
  )
}
               
  




