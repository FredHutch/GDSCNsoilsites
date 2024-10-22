#' Shiny App, UI side
#'
shiny_ui <- function() {

  # Logo used frequently, so make a shorthand here.
  biodigs_logo_header <-
    "<br><a href='https://biodigs.org'><img style = 'display: block; margin-left: auto; margin-right: auto; width: 100%; max-width:500px;' src='https://raw.githubusercontent.com/FredHutch/GDSCNsoilsites/main/www/logo_BioDIGS_final.png'></a><br>"

  #####
  navbarPage(
    title = HTML(
      "<a href='http://gdscn.org'><img style = 'display: block; margin-left: auto; margin-right: auto;' src='https://raw.githubusercontent.com/jhudsl/GDSCN_Book_SARS_Galaxy_on_AnVIL/main/assets/GDSCN_style/logo-gdscn.png' height = '100%'></a>"
    ),
    id = "navBar",
    collapsible = TRUE,
    windowTitle = "BioDIGS",
    position = "fixed-top",
    header = tags$head(includeCSS("www/style.css")),
    # Important for custom css!
    footer = includeHTML("www/footer.html"),

    # Favicon
    tags$head(
      tags$link(rel = "shortcut icon", href = "https://raw.githubusercontent.com/FredHutch/GDSCNsoilsites/main/www/icon.png")
    ),

    # Home Page Tab
    #####
    tabPanel(
      title = "HOME",
      value = "home",
      # Google Analytics
      includeHTML("www/google_analytics.html"),

      HTML(paste0(biodigs_logo_header)),
      HTML("<br>"),
      h2(
        "Empowering underrepresented students in STEM through hands-on research",
        align = "center"
      ),
      p(
        "Dive into genomics & data science: Analyze real soil microbial data from diverse environments.",
        align = "center"
      ),

      p(
        "Uncover secrets of the soil: Learn how microbes impact our health & environment.",
        align = "center"
      ),

      p(
        "Gain cutting-edge skills: Master cloud computing, data analysis, and more.",
        align = "center"
      ),

      p(
        "Join a supportive community: Network with faculty & peers from across the country.",
        align = "center"
      ),
      HTML("<br>"),
      column(
        12,
        actionButton(inputId = 'switch_to_about', label = 'About BioDIGS'),
        align = "center",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      HTML("<br>"),


      # Chunk 2
      HTML("<br>"),
      h2("Get Involved", align = "center"),
      HTML("<br>"),
      column(
        12,
        actionButton(
          inputId = 'ab2',
          label = "Get Your Kit!",
          onclick = "window.open('https://forms.gle/PEe5Kh21GqKHvY7Z8')"
        ),
        align = "center",
        style = "margin-bottom: 10px;",
        style = "margin-top: -10px;"
      ),
      column(
        12,
        actionButton(
          inputId = 'ab2',
          label = HTML(paste0(
            strwrap(
              'Learn more about the Genomic Data Science Community Network',
              width = 40
            ),
            collapse = "</br>"
          )),
          onclick = "window.open('https://gdscn.org/')"
        )
        ,
        align = "center"
        ,
        style = "margin-bottom: 10px;"
        ,
        style = "margin-top: 10px;"
      ),
      HTML("&nbsp;"),
      uiOutput("img")
    ),
    #####
    # Closes the tabPanel called "Home"

    # About Tab + Menu
    #####
    navbarMenu(
      title = "ABOUT",
      tabPanel(
        title = "Why BioDIGS?",
        value = "about",
        # Google Analytics
        includeHTML("www/google_analytics.html"),

        HTML(paste0(biodigs_logo_header)),

        includeMarkdown("www/about.md")
      ),

      # Closes About tab
      tabPanel(
        title = "Team",
        value = "team",
        # Google Analytics
        includeHTML("www/google_analytics.html"),

        HTML(paste0(biodigs_logo_header)),

        includeMarkdown("www/team.md")
      ),
      # Close Team Tab

      tabPanel(
        title = "Sponsors",
        value = "sponsors",
        # Google Analytics
        includeHTML("www/google_analytics.html"),

        HTML(paste0(biodigs_logo_header)),

        includeMarkdown("www/sponsors.md")
      ),
      # Close Sponsors Tab

    ),
    #####
    # Closes the navbarMenu "About"

    # Maps Tab + Menu
    #####
    navbarMenu(
      title = "MAPS",
      tabPanel(
        title = "Sample Map",
        value = "sample_map",
        # Google Analytics
        includeHTML("www/google_analytics.html"),
        checkboxInput(
          "soil_geom_toggle",
          label = 'Show soil survey properties',
          value = FALSE,
          width = NULL
        ),
        # Sample map section
        leafletOutput("sitemap", height = 800)
      ),
      # Closes tabPanel "sample_map"
    ),
    #####
    #  Closes the navbarMenu "Maps"

    # Data & Packages Tab + Menu
    #####
    navbarMenu(
      title = "DATA & PACKAGES",

      tabPanel(
        title = "Site Data",
        value = "site_data",
        # Google Analytics
        includeHTML("www/google_analytics.html"),

        downloadButton('site_data_download', label = 'Download data'),
        HTML("<br><br>"),
        DT::DTOutput("siteDataTable")
      ),
      # Close the Site data tab

      tabPanel(
        title = "Soil Data",
        value = "soil_data",
        # Google Analytics
        includeHTML("www/google_analytics.html"),

        # Plots need to be reassessed
        # uiOutput("testing_plot_region_box"),
        # uiOutput("testing_plot_management_box"),
        # #HTML("<br><br>"),
        # radioButtons(
        #   "testing_response_choice",
        #   "",
        #   c(
        #     "Arsenic" = "As_EPA3051",
        #     "Cadmium" = "Cd_EPA3051",
        #     "Chromium" = "Cr_EPA3051",
        #     "Copper" = "Cu_EPA3051",
        #     "Nickel" = "Ni_EPA3051",
        #     "Lead" = "Pb_EPA3051",
        #     "Zinc" = "Zn_EPA3051",
        #     "Iron" = "Fe_Mehlich3"
        #   )
        # ),
        HTML("<br>"),
        downloadButton('soiltest_download', label = 'Download data'),
        actionButton(
          inputId = 'testing_data_dict',
          label = "Data dictionary",
          icon = icon("th"),
          onclick = "window.open('https://docs.google.com/spreadsheets/d/109xYUM48rjj33B76hZ3bNlrm8u-_S6uyoE_3wSCp0r0/edit#gid=188448677', '_blank')"
        ),
        HTML("<br><br>"),
        DT::DTOutput("soilDataTable"),
        HTML(
          "<br><br><i>Note: Arsenic (As_EPA3051) is not detectable below 3.0 mg/kg. Cadmium (Cd_EPA3051) is not detectable below 0.2 mg/kg.</i>"
        )
      ),
      # Close the Soil data tab

      tabPanel(
        title = "BioDIGSData Package",
        value = "biodigsdata",
        # Google Analytics
        includeHTML("www/google_analytics.html"),

        HTML(paste0(biodigs_logo_header)),

        includeMarkdown("www/biodigsdata.md")
      ),
      # Close the package tab

    ),
    #####
    #  Closes the navbarMenu "Data & Packages"

    # Protocols Tab
    #####
    tabPanel(
      title = "PROTOCOLS",
      value = "protocols",
      # Google Analytics
      includeHTML("www/google_analytics.html"),

      HTML(paste0(biodigs_logo_header)),

      h1("Protocols"),
      HTML("<br>"),
      actionButton(
        inputId = 'ab1',
        label = "Field Protocol (Google Doc)",
        icon = icon("flask"),
        onclick = "window.open('https://docs.google.com/document/d/1oyFaknksSQEpwN7yGEBuGZaDGN96rKGOaovGsbhlGic/edit?usp=sharing', '_blank')"
      ),
      HTML("<br><br>"),
      actionButton(
        inputId = 'ab2',
        label = "Field Protocol (pdf)",
        icon = icon("flask"),
        onclick = "window.open('https://docs.google.com/document/d/1oyFaknksSQEpwN7yGEBuGZaDGN96rKGOaovGsbhlGic/export?format=pdf', '_blank')"
      ),
      HTML("<br><br>"),
      actionButton(
        inputId = 'ab2',
        label = "Protocol Videos",
        icon = icon("video"),
        onclick = "window.open('https://www.youtube.com/playlist?list=PLzgm426KgvrhheloBdlSWshM9v2VvJEcX', '_blank')"
      ),
      HTML("<br><br>"),
      actionButton(
        inputId = 'ab1',
        label = "Field Safety (Google Doc)",
        icon = icon("stethoscope"),
        onclick = "window.open('https://docs.google.com/document/d/1MOeyYVEFyGTxh7cXWhlwMh9FbLIJVr_-eie5FSFpnEs/edit?usp=sharing', '_blank')"
      ),
      HTML("<br><br>"),
      actionButton(
        inputId = 'ab2',
        label = "Field Safety (pdf)",
        icon = icon("stethoscope"),
        onclick = "window.open('https://docs.google.com/document/d/1MOeyYVEFyGTxh7cXWhlwMh9FbLIJVr_-eie5FSFpnEs/export?format=pdf', '_blank')"
      ),
      HTML("<br><br>"),
      actionButton(
        inputId = 'ab2',
        label = "Metadata Form (Google Form)",
        icon = icon("table"),
        onclick = "window.open('https://forms.gle/Z2yH2KBggEF1y4KY9', '_blank')"
      )
    ),
    #####
    #  Closes the tabPanel called "Protocols"

    # Education Tab
    #####
    tabPanel(
      title = "CURRICULA",
      value = "curricula",
      # Google Analytics
      includeHTML("www/google_analytics.html"),

      HTML(paste0(biodigs_logo_header)),

      includeMarkdown("www/education.md"),

      HTML("<br><br>"),
      column(
        12,
        actionButton(
          inputId = 'ab2',
          label = "Launch!",
          onclick = "window.open('https://hutchdatascience.org/GDSCN_BioDIGS_Soil/')"
        )
        ,
        align = "center"
        ,
        style = "margin-bottom: 10px;"
        ,
        style = "margin-top: -10px;"
      ),

      # Create some whitespace around the image
      HTML("&nbsp;"),
      uiOutput("classroom_img", align = "center"),
      HTML("&nbsp;"),
      HTML("&nbsp;"),
      HTML("&nbsp;")
    ),
    #####
    #  Closes the tabPanel called "Education"

    # FAQ Tab
    #####
    tabPanel(
      title = "FAQ",
      value = "faq",
      # Google Analytics
      includeHTML("www/google_analytics.html"),

      includeMarkdown("www/faq.md")
    ),
    #####
    #  Closes the tabPanel called "FAQ"

  ) # Close navbarPage

}
