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

    dashboardPage(

      skin = "green",

      dashboardHeader(title="Soil 🦠 Microbiomes", titleWidth = 300),

      dashboardSidebar(
        width = 300,
        sidebarMenu(
          HTML(
            paste0(
              "<br>",
              "<a href='https://gdscn.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://raw.githubusercontent.com/jhudsl/AnVIL_Template/main/assets/GDSCN_style/logo-gdscn.png' width = '186'></a>",
              "<br>"
            )
          ),
          menuItem(
            "Sample Map",
            tabName = "map",
            icon = icon("thumbtack")
          ),
          menuItem(
            "Browse Site Data",
            tabName = "table",
            icon = icon("table")
          )
        )

      ), # end dashboardSidebar

      dashboardBody(

        tabItems(

          tabItem(tabName = "map",

                  checkboxInput(
                    "soil_geom_toggle",
                    label = 'Show soil survey properties',
                    value = FALSE,
                    width = NULL
                  ),
                  # Sample map section
                  leafletOutput("soilmap", height = 800)

          ),

          tabItem(tabName = "table",

                  DT::DTOutput("siteDataTable")

          )

        ) # end tabItems

      ) # end dashboardBody

    ) # end dashboardPage
  )
}
