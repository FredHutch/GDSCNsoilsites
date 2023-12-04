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

    # Google Analytics
    tags$head(
      HTML(
        "
          <!-- Google tag (gtag.js) -->
          <script async src='https://www.googletagmanager.com/gtag/js?id=G-Q0LZHVXB48'></script>
          <script>
            window.dataLayer = window.dataLayer || [];
            function gtag(){dataLayer.push(arguments);}
            gtag('js', new Date());

            gtag('config', 'G-Q0LZHVXB48');
          </script>
      "
      )
    ),

    dashboardPage(

      skin = "green",

      dashboardHeader(title="BioDIGS 🧬🦠", titleWidth = 300),

      dashboardSidebar(
        width = 300,
        sidebarMenu(
          menuItem(
            "About",
            tabName = "about",
            icon = icon("lightbulb")
          ),
          menuItem(
            "Sample Map",
            tabName = "map",
            icon = icon("thumbtack")
          ),
          menuItem(
            "Soil Lead Map",
            tabName = "leadmap",
            icon = icon("thumbtack")
          ),
          menuItem(
            "Site Data",
            tabName = "table",
            icon = icon("table")
          ),
          menuItem(
            "Soil Testing Data",
            tabName = "soil_table",
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
          ),
          menuItem(
            "FAQs",
            tabName = "faq",
            icon = icon("lightbulb")
          ),
          HTML(
            paste0(
              "<br>",
              "<a href='https://gdscn.org' target='_blank'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://raw.githubusercontent.com/jhudsl/AnVIL_Template/main/assets/GDSCN_style/logo-gdscn.png' width = '186'></a>",
              "<br>"
            )
          )
        )

      ), # end dashboardSidebar

      dashboardBody(

        tabItems(

          tabItem(tabName = "about",

                  includeMarkdown("www/about.md")

          ),

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

          tabItem(tabName = "leadmap",
                  leafletOutput("leadmap", height = 600)

          ),

          tabItem(tabName = "table",

                  downloadButton('site_data_download', label = 'Download data'),
                  HTML("<br><br>"),
                  DT::DTOutput("siteDataTable")

          ),

          tabItem(tabName = "soil_table",

                  uiOutput("testing_plot_region_box"),
                  uiOutput("testing_plot_management_box"),
                  #HTML("<br><br>"),
                  radioButtons("testing_response_choice", "",
                               c("Arsenic" = "As_EPA3051",
                                 "Cadmium" = "Cd_EPA3051",
                                 "Chromium" = "Cr_EPA3051",
                                 "Copper" = "Cu_EPA3051",
                                 "Nickel" = "Ni_EPA3051",
                                 "Lead" = "Pb_EPA3051",
                                 "Zinc" = "Zn_EPA3051")),
                  HTML("<br>"),
                  downloadButton('soiltest_download', label = 'Download data'),
                  actionButton(inputId='testing_data_dict', label="Data dictionary",
                               icon = icon("th"),
                               onclick ="window.open('https://docs.google.com/spreadsheets/d/109xYUM48rjj33B76hZ3bNlrm8u-_S6uyoE_3wSCp0r0/edit#gid=188448677', '_blank')"),
                  HTML("<br><br>"),
                  DT::DTOutput("soilDataTable"),
                  HTML("<br><br><i>Note: Arsenic (As_EPA3051) is not detectable below 3.0 mg/kg. Cadmium (Cd_EPA3051) is not detectable below 0.2 mg/kg.</i>")

          ),

          tabItem(tabName = "conc_table",

                  uiOutput("dna_plot_box"),
                  HTML("<br><br>"),
                  radioButtons("dna_choice", "",
                               c("Concentration" = "qubit_concentration_ng_ul",
                                 "Total amount" = "total_ng")),
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
                  actionButton(inputId='ab2', label="Protocol Videos",
                               icon = icon("video"),
                               onclick ="window.open('https://www.youtube.com/playlist?list=PLzgm426KgvrhheloBdlSWshM9v2VvJEcX', '_blank')"),
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

          ),
          tabItem(tabName = "faq",

                  includeMarkdown("www/faq.md")

          )

        ) # end tabItems

      ) # end dashboardBody

    ) # end dashboardPage
  )
}
