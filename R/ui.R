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
          ),
          menuItem(
            "DNA Concentration Data",
            tabName = "conc_table",
            icon = icon("table")
          ),
          menuItem(
            "Get Materials",
            tabName = "field_materials",
            icon = icon("pencil")
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

                  downloadButton('site_data_download', label = 'Download data'),
                  HTML("<br><br>"),
                  DT::DTOutput("siteDataTable")

          ),

          tabItem(tabName = "conc_table",

                  # uiOutput("dna_plot_box"),
                  # HTML("<br><br>"),
                  # radioButtons("dna_choice", "",
                  #              c("Concentration" = "qubit_concentration_ng_ul",
                  #                "Total amount" = "total_ng")),
                  HTML("<br>"),
                  downloadButton('dnaconc_download', label = 'Download data'),
                  HTML("<br><br>"),
                  DT::DTOutput("dnaconcDataTable")

          ),

          tabItem(tabName = "field_materials",

                  actionButton(inputId='ab1', label="Field Protocol (Google Doc)",
                               icon = icon("flask"),
                               onclick ="window.open('https://docs.google.com/document/d/1oyFaknksSQEpwN7yGEBuGZaDGN96rKGOaovGsbhlGic/edit?usp=sharing', '_blank')"),
                  HTML("<br><br>"),
                  actionButton(inputId='ab2', label="Field Protocol (pdf)",
                               icon = icon("flask"),
                               onclick ="window.open('https://docs.google.com/document/d/1oyFaknksSQEpwN7yGEBuGZaDGN96rKGOaovGsbhlGic/export?format=pdf', '_blank')"),
                  HTML("<br><br>"),
                  actionButton(inputId='ab1', label="Field Safety (Google Doc)",
                               icon = icon("stethoscope"),
                               onclick ="window.open('https://docs.google.com/document/d/1MOeyYVEFyGTxh7cXWhlwMh9FbLIJVr_-eie5FSFpnEs/edit?usp=sharing', '_blank')"),
                  HTML("<br><br>"),
                  actionButton(inputId='ab2', label="Field Safety (pdf)",
                               icon = icon("stethoscope"),
                               onclick ="window.open('https://docs.google.com/document/d/1MOeyYVEFyGTxh7cXWhlwMh9FbLIJVr_-eie5FSFpnEs/export?format=pdf', '_blank')"),
                  HTML("<br><br>"),
                  actionButton(inputId='ab2', label="Metadata Form (Google Form)",
                               icon = icon("table"),
                               onclick ="window.open('https://forms.gle/Z2yH2KBggEF1y4KY9', '_blank')")

          )

        ) # end tabItems

      ) # end dashboardBody

    ) # end dashboardPage
  )
}
