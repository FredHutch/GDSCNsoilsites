#' Title
#'
#' @return
#' @export
#'
#' @examples
getdata <- function() {
  # Set filename and URLs
  google_sheet_url_1 <-
    "https://docs.google.com/spreadsheets/d/1KrPJe9OOGuix2EAvcTfuspAe3kqN-y20Huyf5ImBEl4/edit#gid=804798874"
  google_sheet_url_2 <-
    "https://docs.google.com/spreadsheets/d/1l7wuOt0DZp0NHMAriedbG4EJkl06Mh-G01CrVh8ySJE/edit#gid=804798874"
  soil_file <- "soil_data.csv"

  # Check if the file is/has been written recently.
  # If not, read data in from google sheets, save as `soil_data`
  if (!(file.exists(soil_file))) {
    gs4_auth(cache = ".secrets", email = "hutchdasl@gmail.com")
    soil_data <-
      inner_join(
        read_sheet(google_sheet_url_1),
        read_sheet(google_sheet_url_2),
        by = c(`What is your site name?` = 'site_name'),
        keep = TRUE
      )
    write.csv(soil_data, soil_file)
  } else {
    last_created <- file.info(soil_file)$ctime
    time_since <- lubridate::now() - last_created
    # Check if the file was craeted in the last 24 hrs
    if (time_since > lubridate::as.difftime(24, units = "hours")) {
      gs4_auth(cache = ".secrets", email = "hutchdasl@gmail.com")
      soil_data <-
        inner_join(
          read_sheet(google_sheet_url_1),
          read_sheet(google_sheet_url_2),
          by = c(`What is your site name?` = 'site_name'),
          keep = TRUE
        )
      write.csv(soil_data, soil_file)
    } else {
      soil_data <- read.csv(soil_file)[,-1]
    }
  }

  # Clean up GPS points:
  # Remove parentheses, split column, fix negatives, and make numeric
  gps_points <-
    soil_data %>%
    rename("coord" = 4) %>%
    mutate(coord = str_replace_all(coord, "[//(,//)]*", "")) %>%
    tidyr::separate(coord,
                    into = c("latitude", "longitude"),
                    sep = " ") %>%
    mutate(longitude = case_when(
      as.numeric(longitude) > 0 ~ as.numeric(longitude) * -1,
      TRUE ~ as.numeric(longitude)
    )) %>%
    mutate(latitude = as.numeric(latitude)) %>%
    select(longitude, latitude) %>%
    na.omit() %>%
    as.data.frame()

  # Pull out metadata site names separately
  sitenames <-
    soil_data %>%
    select(2) %>%
    pull()

  # Adjust image urls
  image_urls <-
    soil_data %>%
    mutate(url = paste0('https://drive.google.com/uc?export=view&id=', google_img_id)) %>%
    select(url) %>%
    pull()

  return(list(points = gps_points, sitenames = sitenames, image_urls = image_urls))
}


get_soil_data <- function() {
  # https://data.imap.maryland.gov/datasets/9c48f92b2b4e4663aa78fdd64a1ab010
  soil_type_data <-
    suppressWarnings(
      rbind(
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Needwood_1.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Needwood_2.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Homewood.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_WymanDell.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_StonyRun.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_NDMU.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_DruidHill.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Gwynns.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_HerringRun.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Leakin.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_CabinJohn.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_FarmPark.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_BlackHill.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_RidgeRoad.geojson"
        ),
        rgdal::readOGR(
          "data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_GoshenStream.geojson"
        )
      )
    )

  soil_type_data$CLAY <- round(soil_type_data$CLAY, 1)
  soil_type_data$SAND <- round(soil_type_data$SAND, 1)
  soil_type_data$SILT <- round(soil_type_data$SILT, 1)

  return(soil_type_data)
}
