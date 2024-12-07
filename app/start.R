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

devtools::document()
devtools::load_all()

shinyApp(GDSCNsoilsites:::shiny_ui, GDSCNsoilsites:::shiny_server)
