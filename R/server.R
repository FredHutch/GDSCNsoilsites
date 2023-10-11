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
  ### Server logic

  ### Plot tab logic

  # Reactive leaflet plot that has the option to add soil spatial properties
  display <- reactive({
    if (input$soil_geom_toggle) {
      leadCol <-
        colorFactor(palette = 'RdYlGn',
                    retrieve_plot_data()$lead,
                    reverse = T)

      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = retrieve_plot_data()$points,
          popup = ~ paste0(
            '<img src="',
            as.character(retrieve_plot_data()$image_urls),
            '" alt="image of the site" width="300" height="200">',
            '<br>',
            as.character(retrieve_plot_data()$sitenames)
          ),
          clusterOptions = markerClusterOptions()
        ) %>%
        addPolygons(
          data = get_soil_data(),
          stroke = TRUE,
          smoothFactor = 0.3,
          fillOpacity = 0.3,
          fillColor = "#563d2d",
          color = "#563d2d",
          weight = 1,
          popup = ~ paste0(
            MUNAME,
            "<br>CLAY: ",
            CLAY,
            "%<br>SAND: ",
            SAND,
            "%<br>SILT: ",
            SILT,
            "%"
          )
        )

    } else {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron,
                         options = providerTileOptions(noWrap = TRUE)) %>%
        addMarkers(
          data = retrieve_plot_data()$points,
          popup = ~ paste0(
            '<img src="',
            as.character(retrieve_plot_data()$image_urls),
            '" alt="image of the site" width="300" height="200">',
            '<br>',
            as.character(retrieve_plot_data()$sitenames)
          ),
          clusterOptions = markerClusterOptions()
        )

    }
  })

  output$soilmap <- renderLeaflet({
    display()
  })

  # Reactive leaflet plot that has the option to add soil spatial properties
  display2 <- reactive({
    leadCol <-
      colorFactor(palette = 'RdYlGn',
                  retrieve_plot_data()$lead,
                  reverse = T)


    leaflet() %>%
      setView(-77, 39.1, zoom = 9) %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      addCircleMarkers(
        data = retrieve_plot_data()$points,
        color = ~ leadCol(retrieve_plot_data()$lead),
        radius = ~ 7
      ) %>%
      addLegend(
        'bottomright',
        pal = leadCol,
        values = lead,
        title = 'Lead concentration (mg/kg)',
        opacity = 1
      )
  })

  output$leadmap <- renderLeaflet({
    display2()
  })

  ### Site data table tab logic

  # Create browseable site info table
  output$siteDataTable <- DT::renderDT(get_browseable_data(),
                                       options = list(pageLength = 30, scrollX = TRUE))

  # Downlaod `siteDataTable`
  output$site_data_download <- downloadHandler(
    filename = function() {
      paste("gdscn_soil_site_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_browseable_data(), file, row.names = FALSE)
    }
  )

  ### DNA conc plot, data table tab logic

  # Need to observe rendered plot because it's reactive inside a 'box' element
  observe({
    output$dna_plot <- renderPlot({
      if (input$dna_choice == "total_ng") {
        label_ <- "DNA total amount (ng)"
      } else {
        label_ <- "DNA concentration (Qubit, ng/uL)"
      }
      ggplot(data = get_dna_conc_data(), aes(x = get(input$dna_choice))) +
        geom_histogram(fill = "#73b263") +
        labs(title = label_, x = NULL, y = NULL) +
        theme_classic(base_size = 14)
    })
  })

  # Create the box with the concentration plot
  output$dna_plot_box <- renderUI({
    box(plotOutput("dna_plot"))
  })

  # Create browseable site info table
  output$dnaconcDataTable <- DT::renderDT(get_dna_conc_data(),
                                          options = list(pageLength = 30, scrollX = TRUE))

  # Downlaod `dnaconcDataTable`
  output$dnaconc_download <- downloadHandler(
    filename = function() {
      paste("gdscn_soil_dna_conc_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_dna_conc_data(), file, row.names = FALSE)
    }
  )

  ### Soil Testing Data

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
      ggplot(data = get_browseable_testing_data(),
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
      ggplot(data = get_browseable_testing_data(), aes(
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


  # Create browseable table for soil testing
  output$soilDataTable <-
    DT::renderDT(get_browseable_testing_data(),
                 options = list(pageLength = 30, scrollX = TRUE))

  # Downlaod `soilDataTable`
  output$soiltest_download <- downloadHandler(
    filename = function() {
      paste("gdscn_soil_testing_data-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(get_browseable_testing_data(), file, row.names = FALSE)
    }
  )
}
