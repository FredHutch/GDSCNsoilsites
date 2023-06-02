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

  # r_colors <- rgb(t(col2rgb(colors()) / 255))
  # names(r_colors) <- colors()

  output$soilmap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addMarkers(data = getdata()$points, popup = ~as.character(getdata()$sitenames))
  })
}
