#' Title
#'
#' @return
#' @export
#'
#' @examples
shiny_ui <- function() {
  fluidPage(
    checkboxInput(
      "soil_geom_toggle",
      label = 'Toggle Soil Data',
      value = FALSE,
      width = NULL
    ),
    leafletOutput("soilmap", height = 800)
  )
}
