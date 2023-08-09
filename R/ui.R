#' Title
#'
#' @return
#' @export
#'
#' @examples
shiny_ui <- function() {
  fluidPage(

    # load custom stylesheet
    includeCSS("www/style.css"),

    # checkboxInput(
    #   "soil_geom_toggle",
    #   label = 'Toggle Soil Data',
    #   value = FALSE,
    #   width = NULL
    # ),
    # leafletOutput("soilmap", height = 800),

    dashboardPage(

      skin = "green",

      dashboardHeader(title="GDSCN Soils", titleWidth = 300),

      dashboardSidebar(width = 300,
                       sidebarMenu(
                         # HTML(paste0(
                         #   "<br>",
                         #   "<a href='https://gdscn.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://raw.githubusercontent.com/jhudsl/AnVIL_Template/main/assets/GDSCN_style/logo-gdscn.png' width = '186'></a>",
                         #   "<br>"
                         # )),
                       menuItem("Sample Map", tabName = "map", icon = icon("thumbtack"))
                       )

      ), # end dashboardSidebar

      dashboardBody(

        tabItems(

          tabItem(tabName = "map",

                  checkboxInput(
                    "soil_geom_toggle",
                    label = 'Toggle Soil Data',
                    value = FALSE,
                    width = NULL
                  ),
                  # Sample map section
                  leafletOutput("soilmap", height = 600)

          )

        ) # end tabItems

      ) # end dashboardBody

    ) # end dashboardPage
  )
}
