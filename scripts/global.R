library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinyWidgets)
library(purrr)
library(shinyBS)
library(readxl)
library(plotly)
library(tidyverse)
library(janitor)
library(htmlwidgets)
library(lubridate)
library(scales)
library(viridis)


source("scripts/dashboard_theme.R")
modules <- list.files("modules/", full.names = T)
map(modules, source)


overall <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "Overall - Purchase") %>% 
  mutate(across(where(is.character), trimws))

promotype <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "Overall - PromotypeSIMD") %>% 
  mutate(across(where(is.character), trimws))

online <-  read_excel("data/Kantar data 2019-22.xlsx", sheet = "Overall - OnlineChannels") %>% 
  mutate(across(where(is.character), trimws))

pop <- read_excel("data/Kantar data 2019-22.xlsx", sheet = "Population")

totals_pppd <- overall %>% 
  filter(SIMD == "Total Household") %>% select(-c(Channels, SIMD, `Promotion type`)) %>%pivot_longer(Spend:`Sodium g`) %>%
  left_join(pop, by = c("Year" = "year")) %>%
  mutate(value_pppd = value/population/365)%>%
  select(-c(value, population)) %>%
  pivot_wider(names_from = name, values_from = value_pppd) %>%
  select(-c(ends_with("%"), Trips))
