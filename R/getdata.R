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
