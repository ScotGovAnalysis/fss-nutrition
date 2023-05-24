

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
                                   "</b><p style='font-size:14px'><br>Total spend</p>")),
               style = "metric",
               size = "small",
               width = "100%"
      )
    ),
    column(width = 2,
           bsButton(ns("metric2"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric2_text")),"</b><p style='font-size:14px'><br>Total online spend<p style='font-size:14px'>")),
                    style = "metric",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric3"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric3_text")),"</b><p style='font-size:14px'><br>Spend per person per day")),
                    style = "metric",
                    size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric4"),
                    label = HTML(paste0("<b style='font-size:20px'>", textOutput(ns("metric4_text")),"</b><p style='font-size:14px'><br>Purchased per person per day")),
                    style = "metric",
                  #  icon = icon("thumbs-o-up"),
                   size = "small",
                    width = "100%")
    ),
    column(width = 2,
           bsButton(ns("metric5"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric5_text")),"</b><p style='font-size:14px'>Nutritional volume purchased on a price promotion")),
                  #  icon = icon("tag"),
                    style = "metric",
                    size = "small",
                    width = "100%")
           
           
    ),
    column(width = 2,
           bsButton(ns("metric6"),
                    label = HTML(paste0("<b style='font-size:20px'>",textOutput(ns("metric6_text")),"</b><p style='font-size:14px'>Calories purchased on a price promotion")),
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
                 
                 
               }
               
  )
}




