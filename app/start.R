# not run
library(shiny)
library(leaflet)
library(stringr)
library(googlesheets4)
library(dplyr)
library(GDSCNsoilsites)

#devtools::document()
#devtools::load_all()

shinyApp(shiny_ui, shiny_server)

