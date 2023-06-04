#' Title
#'
#' @return
#' @export
#'
#' @examples
getdata <- function() {
  # Set filename and URL
  google_sheet_url <-
    "https://docs.google.com/spreadsheets/d/1KrPJe9OOGuix2EAvcTfuspAe3kqN-y20Huyf5ImBEl4/edit#gid=804798874"
  soil_file <- "soil_data.csv"

  # Check if the file is/has been written recently.
  # If not, read it in from google sheets, save as `soil_data`
  if (!(file.exists(soil_file))) {
    gs4_auth(cache = ".secrets", email = "hutchdasl@gmail.com")
    soil_data <-
      read_sheet(google_sheet_url)
    write.csv(soil_data, soil_file)
  } else {
    last_created <- file.info(soil_file)$ctime
    time_since <- lubridate::now() - last_created
    if (time_since > 1440) {
      # 60mins * 24 = one day's worth of hours
      gs4_auth(cache = ".secrets", email = "hutchdasl@gmail.com")
      soil_data <-
        read_sheet(google_sheet_url)
      write.csv(soil_data, soil_file)
    } else {
      soil_data <- read.csv(soil_file)[, -1]
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

  return(list(points = gps_points, sitenames = sitenames))
}


get_soil_data <- function() {
  # https://data.imap.maryland.gov/datasets/9c48f92b2b4e4663aa78fdd64a1ab010
  soil_type_data <-
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
      )
    )

  soil_type_data$CLAY <- round(soil_type_data$CLAY, 1)
  soil_type_data$SAND <- round(soil_type_data$SAND, 1)
  soil_type_data$SILT <- round(soil_type_data$SILT, 1)

  return(soil_type_data)
}
