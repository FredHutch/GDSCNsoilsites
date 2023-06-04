#' Title
#'
#' @return
#' @export
#'
#' @examples
getdata <- function() {
  gs4_auth(cache = ".secrets", email = "hutchdasl@gmail.com")
  soil_data <-
    read_sheet(
      "https://docs.google.com/spreadsheets/d/1KrPJe9OOGuix2EAvcTfuspAe3kqN-y20Huyf5ImBEl4/edit#gid=804798874"
    )
  # If you want to save and read..
  # write.csv(soil_data, "soil_data.csv")
  # soil_data <- read.csv("soil_data.csv")[,-1]

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

  # Pull out site names from second column
  sitenames <-
    soil_data %>%
    select(2) %>%
    pull()

  return(list(points = gps_points, sitenames = sitenames))
}


get_soil_data <- function(){
  # https://data.imap.maryland.gov/datasets/9c48f92b2b4e4663aa78fdd64a1ab010
  md_ssurgo1 <- rgdal::readOGR("data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Needwood_1.geojson")
  md_ssurgo2 <- rgdal::readOGR("data/soil_types/Maryland_SSURGO_Soils_-_SSURGO_Soils_Needwood_2.geojson")
  soil_type_data <- rbind(md_ssurgo1, md_ssurgo2)

  soil_type_data$CLAY <- round(soil_type_data$CLAY, 1)
  soil_type_data$SAND <- round(soil_type_data$SAND, 1)
  soil_type_data$SILT <- round(soil_type_data$SILT, 1)

  return(soil_type_data)
}
