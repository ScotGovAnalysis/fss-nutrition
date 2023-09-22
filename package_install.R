




install.packages.scots <- function(pckg_name){
  install.packages(pckg_name, type="win.binary", repos=NULL, contriburl ="file://s1428a/R_Packages/R_4_2_2_Packages")
}

install.packages.scots("purrr")

dashboard_pckgs <- c(
  
  "shiny",
  "shinydashboard",
  "dashboardthemes",
  "shinyWidgets",
  "purrr",
  "shinyBS",
  "readxl",
  "plotly",
  "tidyverse",
  "janitor",
  "htmlwidgets",
  "lubridate",
  "scales",
  "sgplot",
  "shinyalert", 
  "officer", 
  "rvg"

)


pckgs_to_install <- dashboard_pckgs[!dashboard_pckgs %in% installed.packages()]


if(length(pckgs_to_install) > 0){purrr::map(pckgs_to_install, install.packages.scots)}
