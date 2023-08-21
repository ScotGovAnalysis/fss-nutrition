

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
               label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric1_text")),
                                   "</b><p style='font-size:14px'>spent (online and in-store) in total on retail food and drink     
                              </p>")),
               style = "metric",
               size = "small",
               width = "100%"
      )
    ),
    column(width = 2,
           bsButton(ns("metric2"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric2_text")),"</b><p style='font-size:14px'>spent (online and in-store) in total on retail food and drink<p style='font-size:14px'>")),
                    style = "metric",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric3"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric3_text")),"</b><p style='font-size:14px'>spent on retail food and drink, per person, per day")),
                    style = "metric",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric4"),
                    label = HTML(paste0("<b style='font-size:20px'>", textOutput(ns("metric4_text")),"</b><p style='font-size:14px'>purchased from retail, per person,\nper day ")),
                    style = "metric",
                  #  icon = icon("thumbs-o-up"),
                   size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric5"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric5_text")),"</b><p style='font-size:14px'>of retail food and drink volume purchased on price promotion")),
                  #  icon = icon("tag"),
                    style = "metric",
                    size = "small",
                    width = "100%")
           
           
    ),
    column(width = 2,
           bsButton(ns("metric6"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric6_text")),"</b><p style='font-size:14px'>of retail food and drink calories purchased on price promotion")),
                    #icon = icon("tag"),
                    style = "metric",
                    size = "small",
                    width = "100%")
           
           
    )
    ), 
  fluidRow(br()), 
  
  fluidRow(
    box(width = 6, plotlyOutput(ns("plot1"))),
    
    box(width = 6, plotlyOutput(ns("plot2")))
  ), 
  
  fluidRow(
    box(width = 6, plotlyOutput(ns("plot3"))),
  
    box(width = 6, plotlyOutput(ns("plot4")))
  )
  ) 
  
}


# server ------------------------------------------------------------------

yearlyTabServer <- function(id, overall, promotype, online, totals_pppd) {
  
  moduleServer(id,
               function(input, output, session, parent = parent1) {
                 
                 totals <- overall %>% filter(SIMD == "Total Household")

                 # Calculate sample counts to display in buttons
                 
                 output$metric1_text<- renderText({

                  paste0("£", label_number(accuracy = 0.1, scale_cut = cut_short_scale())(totals %>% filter(Year == input$select_year) %>% pull(Spend)))
                   
                 })
                 
                 output$metric2_text <- renderText({
                   
                   online_spend <- online %>% filter(Year == input$select_year) %>% 
                     filter(`Promotion Type` == "Total ONLINE") %>%
                     filter(`Retailer Type` == "Total Market") %>%
                     pull(Spend)
                   
                   paste0("£", label_number(accuracy = 0.1, scale_cut = cut_short_scale())(online_spend))
                 })
                 
                 output$metric3_text <- renderText({
                   
                   paste0("£", label_number(accuracy = 0.01, scale_cut = cut_short_scale())(totals_pppd %>% filter(Year == input$select_year) %>% pull(`Spend`)))
                   
                   
                 })
                 
                 output$metric4_text <- renderText({
                   paste0(comma(round(totals_pppd %>% filter(Year == input$select_year) %>% pull(`Energy kcal`))), " kcal")
                   
                   
                   
                 })
                 output$metric5_text <- renderText({
                   
                   
                   p_nv_pp <- promotype %>%
                     filter(Year == input$select_year, 
                            `Promotion type` == "On Promotion", 
                            SIMD == "Total Household") %>%
                     pull(`Nutritional Volume %`)
                   
                   paste0(round(p_nv_pp), "%")
                          
                          
                 })
                 
                 
                 cal_perc <-  promotype %>%
                     filter(`Promotion type` %in% c("On Promotion", "No promotion")) %>%
                     group_by(Year, SIMD) %>%
                     mutate(total_kcal = sum(`Energy kcal`)) %>%
                     ungroup() %>%
                     mutate(kcal_perc = (`Energy kcal`/total_kcal)*100) %>%
                     filter(`Promotion type` == "On Promotion")
                   
                 
                 
                 
                 output$metric6_text <- renderText({
                   
                   p_kcal_pp <- cal_perc %>%
                     filter(Year == input$select_year, 
                            SIMD == "Total Household")%>%
                     pull(kcal_perc)
                   
                   paste0(round(p_kcal_pp), "%")
                   
                   
                 })
                 
                 
                 
                 
                 output$plot1 <- renderPlotly({
                   
                   online_retail <- online %>%  
                     filter(`Promotion Type`== "Total ONLINE", `Retailer Type` == "Total Market") %>%
                     bind_rows(totals) %>% 
                     select(Channels, Year, Spend, Volume, `Nutritional Volume`, `Energy kcal`) %>%
                     pivot_longer(cols = c(Spend:`Energy kcal`)) %>%
                     pivot_wider(names_from = Channels, values_from = value) %>%
                     mutate(`In-store` = (All - Online)) %>%
                     select(-All) %>%
                     pivot_longer(Online:`In-store`, names_to = "Channel", values_to = "Value") 
                   
                   
                   online_retail %>% 
                     filter(name == "Nutritional Volume") %>%
                     filter(Year == input$select_year) %>%
                     plot_ly(labels = ~Channel, values = ~Value, 
                             hoverinfo = "none",
                             marker = list(colors = c(sg_colour_values[1], sg_colour_values[2]))) %>%
                     add_pie(hole = 0.6, 
                             insidetextorientation = "horizontal") %>%
                     layout(title = paste0("Percentage of food and drink purchased by channel (online or in-store) during ",  input$select_year) ,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE), 
                            margin = list( t = 70))
                  
                   
                 })
                 
                
                   
                   
                 
                 
                 output$plot2 <- renderPlotly({
                   
                   promotype %>%
                     filter(Year == input$select_year) %>%
                     filter(`Promotion type` != "TOTAL SALES",
                            `Promotion type` != "On Promotion") %>%
                     filter(SIMD == "Total Household") %>%
                     mutate(`Promotion type` = as.factor(`Promotion type`), 
                            `Promotion type` = fct_relevel(`Promotion type`, "No promotion"))%>%
                     plot_ly(labels = ~`Promotion type`,
                             values = ~`Nutritional Volume`,

                             marker = list(colors = c(sg_colour_values[1:5]))
                     ) %>%
                     add_pie(hole = 0.6, textposition = "outside") %>%
                     layout(title = paste0("Percentage of retail food and drink calories purchased by price promotion type during ",  input$select_year) ,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                            margin = list( t = 70))
                   
                   
                 })
                 
                 
                 
                 output$plot3 <- renderPlotly({
                   
                   p <- promotype %>% 
                     filter(Year == input$select_year) %>%
                     filter(`Promotion type` != "TOTAL SALES", 
                            `Promotion type` != "On Promotion") %>%
                     filter(!SIMD %in% c("No SIMD", "Total Household"))%>%
                     mutate(`Promotion type` = as.factor(`Promotion type`), 
                            `Promotion type` = fct_relevel(`Promotion type`, c("No promotion", "TPR", "Y for £X", "Other", "MultiBuy")))%>%
                     ggplot() +
                     aes(x = SIMD, y = `Nutritional Volume`, fill = `Promotion type`) +
                     geom_col(position = "fill") +
                     scale_fill_manual(values = c("#12436D", "#28A197", "#F46A25", "#3D3D3D", "#801650")) +
                     scale_y_continuous(labels = scales::percent) +
                     theme_classic() +
                     labs(title = paste0("Percentage of retail food and drink volume purchased by promotion type and SIMD during ",  input$select_year))
                   ggplotly(p)
                   
                 })
                 
                 
                 output$plot4 <- renderPlotly({
                   
                   online %>%
                     filter(SIMD == "Total Household", 
                            `Retailer Type` == "Total Market", 
                            !`Promotion Type` %in% c("Total ONLINE", "On Promotion")) %>%
                     plot_ly(labels = ~`Promotion Type`, 
                             values = ~`Nutritional Volume`, 
                             
                             marker = list(colors = c(sg_colour_values[1:5]))
                     ) %>%
                     add_pie(hole = 0.6, textposition = "outside") %>%
                     layout(title = paste0("Percentage of of online retail food and drink volume purchased by promotion type during ",  input$select_year) ,
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = TRUE), 
                            margin = list( t = 70))
                   
                   
                 })
                 
                 
                 
               }
               
  )
}




