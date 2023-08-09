#' Title
#'
#' @return
#' @export
#'
#' @examples
shiny_ui <- function() {
  fluidPage(
    leafletOutput("soilmap", height = 800)
  )
}








