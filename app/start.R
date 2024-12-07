# not run
library(shiny)
library(shinydashboard)
library(leaflet)
library(stringr)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(ggplot2)
#library(GDSCNsoilsites)

#devtools::document()
#devtools::load_all()

options <- list()
if (!interactive()) {
  options$port = 3838
  options$launch.browser = FALSE
  options$host = "0.0.0.0"

}

shinyApp(GDSCNsoilsites:::shiny_ui, GDSCNsoilsites:::shiny_server, options = options)

