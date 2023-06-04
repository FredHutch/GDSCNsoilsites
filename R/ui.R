#' Title
#'
#' @return
#' @export
#'
#' @examples
shiny_ui <- function() {
  fluidPage(
    leafletOutput("soilmap", height = 800),

    tableOutput("testtable"),

    textOutput("debug_text")
  )
}
