# not run
library(shiny)
library(shinydashboard)
library(leaflet)
library(stringr)
library(googlesheets4)
library(dplyr)
library(tidyr)
library(GDSCNsoilsites)

#devtools::document()
#devtools::load_all()

shinyApp(shiny_ui, shiny_server)

