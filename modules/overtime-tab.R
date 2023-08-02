# This module provides the ui and server for everything on the overall tab.
# The UI takes in an id to link it to the server

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
          column(width = 4, plotlyOutput(ns("plot1"))),
          column(width = 4, plotlyOutput(ns("plot2"))), 
          column(width = 4, plotlyOutput(ns("plot2a")))
        ))),
   fluidRow(box(width = 12,
        fluidRow(
          column(width = 6, plotlyOutput(ns("plot3"))),
          column(width = 6, plotlyOutput(ns("plot4")))
        )),
    
               box(width = 12,
                   column(width = 6, plotlyOutput(ns("plot5"))),
                   column(width = 6, plotlyOutput(ns("plot6"))),
             )
  ))
}


# server ------------------------------------------------------------------
# the server takes in an id and the data. 
# it renders all of the plots and tables on the overall tab

overtimeTabServer <- function(id, overall, promotype, online, totals_pppd, category) {
  moduleServer(id, function(input, output, session) {
    
    
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
    
    
    
    
    output$trips <- renderPlotly({
      
      
      p5 <- overall %>% filter(SIMD == "Total Household") %>% 
        ggplot() +
        aes(x = Year, y = Trips) +
        geom_col(color = "#babd8b", fill = "#babd8b") +
        theme_classic() +
        labs(y = "Number of trips per household", 
             title = "Average number of annual retail food and drink trips\nper househould in Scotland")
      
      ggplotly(p5)
      
      
    })
    
 
    
    output$plot1 <- renderPlotly({
      p1 <-   overall %>%
        filter(SIMD ==  "Total Household") %>% 
        mutate(Year = as.character(Year)) %>%
        ggplot()+
        aes(x = Year, y = get(input$metric)) +
        geom_bar(stat = "identity", fill = "#3f2a56") +
        scale_y_continuous(labels = unit_format(unit = "M", scale = 1e-6)) +
        theme_classic() +
        labs(y = input$metric, 
             title = paste0("Total annual ", str_to_lower(input$metric), " purchased on retail food and drink in Scotland"))
      ggplotly(p1)
    })
    
    output$plot2 <- renderPlotly({
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
             title = paste0("Total annual online ", str_to_lower(input$metric),  " purchased on food and drink, by retailer type, in Scotland" ))
      ggplotly(p2)
      
    })
    
    
    
    output$plot2a <- renderPlotly({
      
      var_name <- paste0(input$metric, " %")
      category %>% 
        filter(SIMD == "Total Household") %>%
        filter(`F&D Category` != "Total Food & Drink") %>%
        filter(food_groups != "Other") %>%
        group_by(food_groups, Year) %>%
        summarise(var_name := sum(get(var_name))) %>%
        plot_ly(x = ~Year,  
                y = ~var_name, 
                color = ~ food_groups, 
                type = "scatter", 
                mode = "lines", 
                colors = c( "#12436D","#28A197","#801650" ,
                            "#F46A25","#3D3D3D" )) %>%
        layout(title = paste0("Annual retail purchase of food and drink categories as a percentage of total annual ", str_to_lower(input$metric)),
               yaxis = list(title = "% of total food and drink"))
      
      
    })
    
    
    
    output$plot3 <- renderPlotly({
      
      
      promotype %>%
        mutate(Year = as.character(Year)) %>%
        filter(SIMD != "No SIMD") %>%
        filter(`Promotion type` == "On Promotion") %>%
        plot_ly(x = ~Year, 
                y = ~`Nutritional Volume %`, 
                color = ~ SIMD, 
                type = "scatter", 
                mode = "lines",
                colors = c( "#12436D","#28A197","#801650" ,
                                             "#F46A25","#3D3D3D","#A285D1",
                                             "#2073BC" ))%>%
        layout(title = list(text = paste0("Total annual retail purchase of food and drink purchased on price promotion in Scotland,\nby SIMD"), 
                            xanchor = "left", 
                            x = 0), 
               yaxis = list(title = "Percentage volume purchased on promotion (%)"))
      
    })
    
    
    
    output$plot4 <- renderPlotly({
      
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
        scale_fill_discrete_sg("main6") +
        theme_classic() +
        labs(y = "Percentage volume purchased on promotion (%)", 
             title = "Total annual retail purchase of food and drink purchased on price promotion\nin Scotland, by promotion type")
      
      
    })
    
    
    
    
    
    output$plot5 <- renderPlotly({
      
      
      
      totals_pppd %>% 
        select(Year, `Energy kcal`:`Sodium g`) %>% 
        pivot_longer(`Energy kcal`:`Sodium g`, names_to = "Nutrient") %>% 
        filter(Nutrient != "Energy kcal") %>%
        mutate(Year = as.character(Year)) %>%
        plot_ly(x = ~Year, 
                y = ~value, 
                color = ~ Nutrient, 
                type = "scatter", 
                mode = "lines",
                colors = c( "#12436D","#28A197","#801650" ,
                             "#F46A25","#3D3D3D","#A285D1",
                             "#2073BC" )) %>%
        layout(title = "Average annual retail purchase of key nutrients, per capita, per day in Scotland",
               yaxis = list(title = "Grams (g)"))
      
      
    })
    
    

    
  })
}