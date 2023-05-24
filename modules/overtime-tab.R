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
          column(width = 6, plotlyOutput(ns("plot1"))),
          column(width = 6, plotlyOutput(ns("plot2")))
        ))),
    box(width = 12,
        fluidRow(
          column(width = 6, plotlyOutput(ns("plot3"))),
          column(width = 6, plotlyOutput(ns("plot4")))
        )),
    fluidRow(column(width = 2),
             column(
               width = 8,
               box(width = 12,
                   plotlyOutput(ns("plot5"))),
               column(width = 2)
             ))
  )
}


# server ------------------------------------------------------------------
# the server takes in an id and the data. 
# it renders all of the plots and tables on the overall tab

overtimeTabServer <- function(id, overall, promotype, online, totals_pppd) {
  moduleServer(id, function(input, output, session) {
    
    
    output$spend_pppd <- renderPlotly({
      
      
      
      p7 <- totals_pppd %>%
        ggplot() +
        aes(x = Year, y = Spend) +
        geom_col(color = "#607875", fill = "#607875") +
        theme_classic() +
        labs(title = "Average spend per capita per day in Scotland", 
             y = "Spend (£)")
      
      ggplotly(p7)
      
    })
    
    
    output$kcal_pppd <- renderPlotly({
      
      p8 <- totals_pppd %>%
        ggplot() +
        aes(x = Year, y = `Energy kcal`) +
        geom_col(color = "#005e5d", fill = "#005e5d") +
        theme_classic() +
        labs(title = "Average purchase of calories (kcal) per capita \nper day in Scotland", 
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
             title = "Average number of trips per household per year")
      
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
             title = paste0("Overall ", input$metric, " in Scotland per year"))
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
             title = paste0("Online purchases by retailer type" ))
      ggplotly(p2)
      
    })
    
    
    output$plot3 <- renderPlotly({
      
      
      promotype %>%
        mutate(Year = as.character(Year)) %>%
        filter(`Promotion type` == "On Promotion") %>%
        plot_ly(x = ~Year, 
                y = ~`Nutritional Volume %`, 
                color = ~ SIMD, 
                type = "scatter", 
                mode = "lines")%>%
        layout(title = list(text = paste0("Percentage purchased on a price promotion by SIMD"), 
                            xanchor = "left", 
                            x = 0), 
               yaxis = list(title = "Percentage (%)"))
      
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
        scale_fill_viridis_d() +
        theme_classic() +
        labs(y = "Percentage of nutritional volume", 
             title = "Proportion of nutritional volume purchased by promotion type")
      
      
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
                colors = viridis_pal()(7)) %>%
        layout(title = "Average purchase of nutritional component per person per day in Scotland",
               yaxis = list(title = "Grams"))
      
      
    })
    
  })
}