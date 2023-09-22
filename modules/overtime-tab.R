# This module provides the ui and server for everything on the trends over time tab
# Plots are labelled left to right and top to bottom

overtimeTabUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(box(
      width = 12,
      column(width = 4, plotlyOutput(ns("spend_pppd"))),
      column(width = 4, plotlyOutput(ns("kcal_pppd"))),
      column(width = 4, plotlyOutput(ns("trips")))
    )),
    box(width = 12,
        fluidRow(column(
          width = 12,
          radioGroupButtons(
            ns("metric"),
            label = "Select the metric for the y-axis: ",
            choices = c("Spend", "Nutritional Volume", "Energy kcal")
          )
        )),
        column(width = 12,    fluidRow(
          column(width = 3, plotlyOutput(ns("total_sp_nv_kc"))),
          column(width = 4, plotlyOutput(ns("online_retailer"))), 
          column(width = 5, plotlyOutput(ns("category_sp_nv")))
        ))),
   fluidRow(box(width = 12,
        fluidRow(
          column(width = 6, plotlyOutput(ns("nv_simd"))),
          column(width = 6, plotlyOutput(ns("nv_promo")))
        )),
    
               box(width = 12,
                   column(width = 6, plotlyOutput(ns("nutrients"))),
           #        column(width = 6, plotlyOutput(ns("plot6"))),
             )
  ))
}


# server ------------------------------------------------------------------
# the server takes in an id and the data. 
# it renders all of the plots and tables on the overall tab

overtimeTabServer <- function(id, overall, promotype, online, totals_pppd, category) {
  moduleServer(id, function(input, output, session) {
  
    

# Plot 1 - average annual spend on retail food and drink per capit --------

      
    
    output$spend_pppd <- renderPlotly({
      
      
      
      p7 <- totals_pppd %>%
        ggplot() +
        aes(x = Year, y = Spend) +
        geom_col(color = "#607875", fill = "#607875") +
        theme_classic() +
        labs(title = "Average annual spend on retail food and drink,\nper capita, per day in Scotland", 
             y = "Spend (£)")
      
      ggplotly(p7)
      
    })
    
  

# Plot 2 - kcal per capita per day  ---------------------------------------

      
    output$kcal_pppd <- renderPlotly({
      
      p8 <- totals_pppd %>%
        ggplot() +
        aes(x = Year, y = `Energy kcal`) +
        geom_col(color = "#005e5d", fill = "#005e5d") +
        theme_classic() +
        labs(title = "Average annual purchase of calories from retail\nper capita per day in Scotland", 
             y = "Energy (kcal)")
      
      ggplotly(p8)
      
    })
    
    
    

# Plot 3 - trips per household --------------------------------------------

    
    output$trips <- renderPlotly({
      
      
      p5 <- overall %>% filter(SIMD == "Total Household") %>% 
        ggplot() +
        aes(x = Year, y = Trips) +
        geom_col(color = "#babd8b", fill = "#babd8b") +
        theme_classic() +
        labs(y = "Number of trips per household", 
             title = "Average number of annual retail food and drink trips\nper household in Scotland")
      
      ggplotly(p5)
      
      
    })
    
 


# Plot 4 - annual spend/kcal/nut vol on retail in Scotland  -------------------------------------

      
    
    output$total_sp_nv_kc <- renderPlotly({
      p1 <-   overall %>%
        filter(SIMD ==  "Total Household") %>% 
        mutate(Year = as.character(Year)) %>%
        ggplot()+
        aes(x = Year, y = get(input$metric)) +
        geom_bar(stat = "identity", fill = "#3f2a56") +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
        theme_classic() +
        labs(y = input$metric, 
             title = str_wrap(paste0("Total annual ", str_to_lower(input$metric), " purchased on retail food and drink in Scotland"), 45))
      ggplotly(p1)
    })

    
    

# Plot 5 - spend/kcal/nut vol by retailer type online  --------------------

        
    output$online_retailer <- renderPlotly({
      p2 <- online %>%
        filter(`Promotion Type` == "Total ONLINE") %>%
        filter(`Retailer Type` != "Total Market") %>%
        ggplot() +
        aes(x = Year, y = get(input$metric), fill = `Retailer Type`) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("#005e5d", "#babd8b")) +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
        theme_classic() +
        labs(y = input$metric, 
             title = str_wrap(paste0("Total annual online ", str_to_lower(input$metric),  " purchased on food and drink, by retailer type, in Scotland" ), 45))
      ggplotly(p2)
      
    })
    
    

    

# Plot 6 - spend/nut vol by category (meat, fish, veg, discretionary, additional) --------------------------------------

        
    output$category_sp_nv <- renderPlotly({
      

    # Figures for percentage kcal are not available - can be calculated but placed as a low priority development for now
      validate(
        need(input$metric != "Energy kcal", " ")
      )
      
      var_name <- paste0(input$metric, " %")
      
      category %>% 
        filter(SIMD == "Total Household") %>%
        filter(`F&D Category` != "Total Food & Drink") %>%
        filter(food_groups != "Other") %>%
        group_by(food_groups, Year) %>%
        summarise(var_name := sum(get(var_name))) %>%
        # plot_ly(x = ~Year,  
        #         y = ~var_name, 
        #         color = ~ food_groups, 
        #         type = "scatter", 
        #         mode = "lines", 
        #         colors = c( "#12436D","#28A197","#801650" ,
        #                     "#F46A25","#3D3D3D" )) %>%
        # layout(title = paste0("Annual retail purchase of food and drink categories as a\npercentage of total annual ", str_to_lower(input$metric)),
        #        yaxis = list(title = "% of total food and drink"))
        # 
      
      ggplot() +
        aes(x = Year, y = var_name, color = food_groups, group = food_groups) +
        geom_line() +
        scale_colour_discrete_sg("main6", palette_type = "af", name = " " ) +
        theme_classic() +
        labs(y = "Percentage of total food and drink (%)", 
             title = paste0("Annual retail purchase of food and drink categories as a\npercentage of total annual ", str_to_lower(input$metric)))
      
      
      
    })
    
    

# Plot 7 - NV purchased on price promotion by SIMD --------------------------------


        
    output$nv_simd <- renderPlotly({
      
      # 
      # promotype %>%
      #   mutate(Year = as.character(Year)) %>%
      #   filter(SIMD != "No SIMD") %>%
      #   filter(`Promotion type` == "On Promotion") %>%
      #   plot_ly(x = ~Year, 
      #           y = ~`Nutritional Volume %`, 
      #           color = ~ SIMD, 
      #           type = "scatter", 
      #           mode = "lines",
      #           colors = c( "#12436D","#28A197","#801650" ,
      #                                        "#F46A25","#3D3D3D","#A285D1",
      #                                        "#2073BC" ))%>%
      #   layout(title = list(text = paste0("Total annual retail purchase of food and drink purchased on price promotion in Scotland,<br />by SIMD <br />"), 
      #                        x = 0, y = 1.2), 
      #          yaxis = list(title = "Percentage nutritional volume purchased on promotion (%)"), 
      #          margin = list(t = 100))
      # 
      promotype %>%
        mutate(Year = as.character(Year)) %>%
        filter(SIMD != "No SIMD") %>%
        filter(`Promotion type` == "On Promotion") %>%
      ggplot() +
        aes(x = Year, y = `Nutritional Volume %`, color = SIMD, group = SIMD) +
        geom_line() +
        scale_colour_discrete_sg("main6", palette_type = "af") +
        theme_classic() +
        labs(y = "Percentage nutritional volume\npurchased on promotion (%)", 
             title = "Total annual retail purchase of food and drink purchased on price promotion in Scotland,<br />by SIMD <br />")
      
      
      
    })
    
    
  

# Plot 8 - NV on price promo by promo type --------------------------------

      
    output$nv_promo <- renderPlotly({
      
      promotype %>%
        filter(SIMD ==  "Total Household", 
               #   `Promotion type` %in% c("No promotion", "On Promotion")) %>%
               !`Promotion type` %in% c("TOTAL SALES", "On Promotion")) %>%
        mutate(`Promotion type` = as.factor(`Promotion type`),
               `Promotion type` = fct_relevel(`Promotion type`, c("No promotion"))) %>%
        
        ggplot() +
        aes(x = Year, y = `Nutritional Volume`, fill = `Promotion type`) +
        geom_col(position = "fill") +
        scale_y_continuous(labels = scales::percent) +
        scale_fill_discrete_sg("main6", palette_type = "af") +
        theme_classic() +
        labs(y = "Percentage nutritional volume\npurchased on promotion (%)", 
             title = "Total annual retail purchase of food and drink purchased on price promotion\nin Scotland, by promotion type")
      
      
    })
    
    
  

# Plot 9 - key nutrients per capita per day  ------------------------------

    
    
    
    output$nutrients <- renderPlotly({
      
      dat <-       totals_pppd %>% 
        select(Year, `Energy kcal`:`Sodium g`) %>% 
        pivot_longer(`Energy kcal`:`Sodium g`, names_to = "Nutrient") %>% 
        filter(Nutrient != "Energy kcal") %>%
        mutate(Year = as.character(Year))
      
      ann_labs <- dat %>%
        group_by(Nutrient) %>%
        mutate(min_year = min(Year)) %>%
        filter(Year == max(Year))
      
      dat %>%
        # plot_ly(x = ~Year, 
        #         y = ~value, 
        #         color = ~ Nutrient, 
        #         type = "scatter", 
        #         mode = "lines",
        #         colors = c( "#12436D","#28A197","#801650" ,
        #                      "#F46A25","#3D3D3D","#A285D1",
        #                      "#2073BC" )) %>%
        # layout(title = "Average annual retail purchase of key nutrients, per capita, per day in Scotland",
        #        yaxis = list(title = "Grams (g)"))
      
      ggplot() +
        aes(x = Year, y = value, color = Nutrient, group = Nutrient) +
        geom_line() +
        scale_colour_manual(values = c( "#12436D","#28A197","#801650" ,
                                                                "#F46A25","#3D3D3D","#A285D1",
                                                                "#2073BC" )) +
        theme_classic() +
        labs(y = "Grams (g)", 
             title =  "Average annual retail purchase of key nutrients, per capita, per day in Scotland") 
     #   geom_label(data = ann_labs, aes(x = Year, y = value, label = Nutrient, color = Nutrient), hjust = 0, vjust = 0.5, nudge_x = 0.1, label.size = NA)+
     #   scale_x_discrete(expand = c(0, 2)) +
      #  theme(legend.position = "none")
      
        
        ########################################### COLOR PALETTE! Fix this. 
    })
    
    

    
  })
}