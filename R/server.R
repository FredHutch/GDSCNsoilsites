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

  display <- reactive({
    if (input$soil_geom_toggle){

      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = getdata()$points,
          # popup = ~ paste0(
          #   '<img src="',
          #   as.character(getdata()$image_urls),
          #   '" alt="image of the site" width="300" height="200">',
          #   '<br>',
          #   as.character(getdata()$sitenames)
          # ),
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
          popup = ~paste0(MUNAME, "<br>CLAY: ", CLAY, "%<br>SAND: ", SAND, "%<br>SILT: ", SILT, "%")
        )

    } else {

      leaflet() %>%
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = getdata()$points,
          # popup = ~ paste0(
          #   '<img src="',
          #   as.character(getdata()$image_urls),
          #   '" alt="image of the site" width="300" height="200">',
          #   '<br>',
          #   as.character(getdata()$sitenames)
          # ),
          clusterOptions = markerClusterOptions()
        )

    }
  })

  output$soilmap <- renderLeaflet({
    display()
  })

}
