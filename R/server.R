#' Title
#'
#' @param input
#' @param output
#' @param session
#'
#' @return
#' @export
#'
#' @examples
shiny_server <- function(input, output, session) {
  ### Server logic

  ### Plot tab logic

  # Reactive leaflet plot that has the option to add soil spatial properties
  display <- reactive({
    if (input$soil_geom_toggle) {
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = retrieve_plot_data()$points,
          popup = ~ paste0(
            '<img src="',
            as.character(retrieve_plot_data()$image_urls),
            '" alt="image of the site" width="300" height="200">',
            '<br>',
            as.character(retrieve_plot_data()$sitenames)
          ),
          clusterOptions = markerClusterOptions()
        ) %>%
        addPolygons(
          data = get_soil_data(),
          stroke = TRUE,
          smoothFactor = 0.3,
          fillOpacity = 0.3,
          fillColor = "#563d2d",
          color = "#563d2d",
          weight = 1,
          popup = ~ paste0(
            MUNAME,
            "<br>CLAY: ",
            CLAY,
            "%<br>SAND: ",
            SAND,
            "%<br>SILT: ",
            SILT,
            "%"
          )
        )

    } else {
      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = retrieve_plot_data()$points,
          popup = ~ paste0(
            '<img src="',
            as.character(retrieve_plot_data()$image_urls),
            '" alt="image of the site" width="300" height="200">',
            '<br>',
            as.character(retrieve_plot_data()$sitenames)
          ),
          clusterOptions = markerClusterOptions()
        )

    }
  })

  output$soilmap <- renderLeaflet({
    display()
  })

  ### Site data table tab logic

  # Create browseable site info table
  output$siteDataTable <- DT::renderDT(get_browseable_data(),
                                       options = list(pageLength = 30))

  # Downlaod `siteDataTable`
  output$site_data_download <- downloadHandler(
    filename = function() {
      paste("gdscn_soil_site_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_browseable_data(), file, row.names = FALSE)
    }
  )

}
