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
  output$soilmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(
        data = getdata()$points,
        popup = ~ as.character(getdata()$sitenames),
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
  })
}
