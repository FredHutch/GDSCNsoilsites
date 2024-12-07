#' Shiny App, server side
#'
#' @param input inputs from UI
#' @param output outputs to UI
#' @param session the individual app session
#'
shiny_server <- function(input, output, session) {
  ### Server logic

  ### Go to specific tabs and allow them to have their own url
  # Helpful link:
  # https://stackoverflow.com/questions/70080803/uri-routing-for-shinydashboard-using-shiny-router/70093686#70093686

  observeEvent(input$navBar, {
    clientData <- reactiveValuesToList(session$clientData)
    newURL <- with(clientData, paste0(url_protocol, "//", url_hostname, ":", url_port, url_pathname, "#", input$navBar))
    updateQueryString(newURL, mode = "replace", session)
  })

  observe({
    currentTab <- sub("#", "", session$clientData$url_hash)
    if(!is.null(currentTab)){
      updateTabItems(session, "navBar", selected = currentTab)
    }
  })

  ### Images

  # Student Image
  output$img <- renderUI({
    tags$img(src = "https://raw.githubusercontent.com/FredHutch/GDSCNsoilsites/main/www/students.png", height="100%", width="100%")
  })

  # Activity Image
  output$classroom_img <- renderUI({
    tags$img(src = "https://raw.githubusercontent.com/FredHutch/GDSCNsoilsites/main/www/classroom.png", height="80%", width="80%")
  })

  ### Plot tab logic

  # Reactive leaflet plot to map SITES. Option to add soil spatial properties
  site_map_leaflet_display <- reactive({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = retrieve_plot_data()$points,
          popup = ~ paste0(
            '<img src="',
            as.character(retrieve_plot_data()$image_urls),
            '" alt="image of the site" height="200px">',
            '<br>',
            as.character(retrieve_plot_data()$sitenames),
            '<br>',
            "Lead: ", as.character(retrieve_plot_data()$partner_faculty)
          ),
          clusterOptions = markerClusterOptions(),
          popupOptions = popupOptions(maxWidth = "200px")
        )
  })

  # Display the SITES leaflet plot
  output$sitemap <- renderLeaflet({
    site_map_leaflet_display()
  })

  ### Data tab logic

  # Create browseable "site data" table
  output$siteDataTable <- DT::renderDT(get_browseable_site_data(),
                                       options = list(pageLength = 30, scrollX = TRUE))

  # Downlaod `siteDataTable` "site data" table
  output$site_data_download <- downloadHandler(
    filename = function() {
      paste("gdscn_soil_site_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_browseable_site_data(), file, row.names = FALSE)
    }
  )

  # Need to observe rendered plot because it's reactive inside a 'box' element
  observe({
    output$testing_plot_region <- renderPlot({
      if (input$testing_response_choice == "As_EPA3051")
        response_ <- "Arsenic"
      if (input$testing_response_choice == "Cd_EPA3051")
        response_ <- "Cadmium"
      if (input$testing_response_choice == "Cr_EPA3051")
        response_ <- "Chromium"
      if (input$testing_response_choice == "Cu_EPA3051")
        response_ <- "Copper"
      if (input$testing_response_choice == "Ni_EPA3051")
        response_ <- "Nickel"
      if (input$testing_response_choice == "Pb_EPA3051")
        response_ <- "Lead"
      if (input$testing_response_choice == "Zn_EPA3051")
        response_ <- "Zinc"
      if (input$testing_response_choice == "Fe_Mehlich3")
        response_ <- "Iron"
      ggplot(data = get_browseable_soil_testing_data(),
             aes(
               x = get(input$testing_response_choice),
               group = region,
               color = region,
               fill = region
             )) +
        geom_density(alpha = 0.6) +
        scale_color_manual(values = c("#73b263", "#739999")) +
        scale_fill_manual(values = c("#73b263", "#739999")) +
        labs(
          title = paste0(response_, " (mg/kg) by region"),
          x = NULL,
          y = NULL,
          colour = NULL,
          fill = NULL
        ) +
        theme_classic(base_size = 14) +
        theme(legend.position = c(0.8, 1)) # top right inset position
    })
  })

  observe({
    output$testing_plot_management <- renderPlot({
      if (input$testing_response_choice == "As_EPA3051")
        response_ <- "Arsenic"
      if (input$testing_response_choice == "Cd_EPA3051")
        response_ <- "Cadmium"
      if (input$testing_response_choice == "Cr_EPA3051")
        response_ <- "Chromium"
      if (input$testing_response_choice == "Cu_EPA3051")
        response_ <- "Copper"
      if (input$testing_response_choice == "Ni_EPA3051")
        response_ <- "Nickel"
      if (input$testing_response_choice == "Pb_EPA3051")
        response_ <- "Lead"
      if (input$testing_response_choice == "Zn_EPA3051")
        response_ <- "Zinc"
      if (input$testing_response_choice == "Fe_Mehlich3")
        response_ <- "Iron"
      ggplot(data = get_browseable_soil_testing_data(), aes(
        x = get(input$testing_response_choice),
        group = type,
        color = type,
        fill = type
      )) +
        geom_density(alpha = 0.6) +
        scale_color_manual(values = c("#73b263", "#739999")) +
        scale_fill_manual(values = c("#73b263", "#739999")) +
        labs(
          title = paste0(response_, " (mg/kg) by management type"),
          x = NULL,
          y = NULL,
          colour = NULL,
          fill = NULL
        ) +
        theme_classic(base_size = 14) +
        theme(legend.position = c(0.8, 1)) # top right inset position
    })
  })

  # Create the boxes with the plots
  output$testing_plot_management_box <- renderUI({
    box(plotOutput("testing_plot_management"))
  })
  output$testing_plot_region_box <- renderUI({
    box(plotOutput("testing_plot_region"))
  })


  # Create browseable table for soil testing data
  output$soilDataTable <-
    DT::renderDT(get_browseable_soil_testing_data(),
                 options = list(pageLength = 30, scrollX = TRUE))

  # Downlaod `soilDataTable` table for soil testing data
  output$soiltest_download <- downloadHandler(
    filename = function() {
      paste("gdscn_soil_testing_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_browseable_soil_testing_data(), file, row.names = FALSE)
    }
  )

}
