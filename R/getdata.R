
#' Gets relevant data from snapshot file.
#'
#' @return an uncleaned `data.frame`
#' @export
#'
#' @examples
#' getdata()
getdata <- function() {
  # Set filename
  soil_file <- "soil_data.csv"

  # Check if the file is/has been written.
  # If not, read it in, save as `soil_data`
  if (!(file.exists(soil_file))) {
    soil_data <-
      read.csv("data/snapshots/BioDIGS_20241016.csv")
    write.csv(soil_data, soil_file)
  } else {
    soil_data <- read.csv(soil_file)[,-1]
  }

  return(soil_data)

}


#' Cleans data to make GPS coordinates consistent.
#'
#' @param the_data
#'
#' @return Cleaned `data.frame`
#' @export
#'
#' @examples
#' clean_gps_points(getdata())
clean_gps_points <- function(the_data) {
  # Clean up GPS points:
  # Remove parentheses, split column, fix negatives, and make numeric
  the_data <- the_data %>%
    filter(gps != "Not yet provided") %>%
    mutate(gps = str_replace_all(gps, "[//(,//)]*", "")) %>%
    tidyr::separate(gps,
                    into = c("latitude", "longitude"),
                    sep = " ") %>%
    mutate(longitude = case_when(
      as.numeric(longitude) > 0 ~ as.numeric(longitude) * -1,
      TRUE ~ as.numeric(longitude)
    )) %>%
    mutate(latitude = as.numeric(latitude))

  return(the_data)
}


#' Clean the site name details in case we don't want "rep 1" or "rep 2" included
#'
#' @param the_data
#'
#' @return Cleaned `data.frame`
#' @export
#'
#' @examples clean_site_name_rep_detail(getdata())
clean_site_name_rep_detail <- function(the_data) {
  # Clean up site_name_rep_detail column
  # Keep only unique sites, and clean site details so no rep information is included.
  the_data <-
    the_data[match(unique(the_data$site_id), the_data$site_id), ] %>%
    mutate(site_name_rep_detail = str_remove(site_name_rep_detail, "( rep 1| Rep 1| rep #1| Rep #1)$")) %>%
    mutate(site_name_rep_detail = str_remove(site_name_rep_detail, "( rep 2| Rep 2| rep #2| Rep #2)$"))

  return(the_data)
}


#' Partition out the data into a list for quick use in plots.
#'
#' @return a `list` of gps coordinates, site names, and image urls
#' @export
#'
#' @examples
#' # Get gps points as a dataframe:
#' retrieve_plot_data()$points
#'
#' # Get site names as a vector:
#' retrieve_plot_data()$sitenames
#'
#' # Get urls for images as a vector:
#' retrieve_plot_data()$image_urls
retrieve_plot_data <- function() {
  # Read in, clean GPS points / clean reps from site descriptions
  soil_data <-
    getdata() %>%
    clean_gps_points() %>%
    clean_site_name_rep_detail()

  # Extract gps coordinates
  gps_points <-
    soil_data %>%
    clean_site_name_rep_detail() %>%
    select(longitude, latitude) %>%
    as.data.frame()

  # Pull out metadata site names separately
  sitenames <-
    soil_data %>%
    clean_site_name_rep_detail() %>%
    mutate(site_name = paste(site_id, "-", site_name_rep_detail)) %>%
    select(site_name) %>%
    pull()

  # Adjust image urls
  image_urls <-
    soil_data %>%
    clean_site_name_rep_detail() %>%
    dplyr::mutate(url = paste0(
      'https://lh3.googleusercontent.com/d/',
      google_img_id
    )) %>%
    pull(url)

  return(list(
    points = gps_points,
    sitenames = sitenames,
    image_urls = image_urls
  ))
}


#' Produce data in a nice clean format for browsing & downloading
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' get_browseable_site_data()
get_browseable_site_data <- function() {
  soil_data_to_browse <-
    clean_gps_points(getdata())

  soil_data_to_browse <-
    soil_data_to_browse[match(unique(soil_data_to_browse$site_id),
                              soil_data_to_browse$site_id), ] %>%
    rename(site_description = site_name_rep_detail) %>%
    mutate(date_sampled = lubridate::mdy(collection_date)) %>%
    select(site_id,
           site_description,
           mgmt_type,
           date_sampled,
           latitude,
           longitude)

  return(soil_data_to_browse)
}


#' Produce soil testing data in a nice clean format for browsing on the app.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' get_browseable_soil_testing_data()
get_browseable_soil_testing_data <- function() {
  testing_data_to_browse <-
    getdata() %>%
    filter(bulk_type == "Soil") %>%
    clean_gps_points() %>%
    clean_site_name_rep_detail()

  testing_data_to_browse <-
    testing_data_to_browse %>%
    select(
      site_id,
      site_name_rep_detail,
      sample_id,
      origin,
      mgmt_type,
      tidyr::ends_with("EPA3051"),
      water_pH,
      OM_by_LOI_pct,
      tidyr::ends_with("Mehlich3"),
      Est_CEC,
      Base_Sat_pct,
      P_Sat_ratio
    ) %>%
    # As can't be detected lower than 3.0
    mutate(As_EPA3051 = case_when(As_EPA3051 == "< 3.0" ~ "0", TRUE ~ As_EPA3051)) %>%
    # Cd can't be detected lower than 0.2
    mutate(Cd_EPA3051 = case_when(Cd_EPA3051 == "< 0.2" ~ "0", TRUE ~ Cd_EPA3051))

  return(testing_data_to_browse)
}

# Comment here to trigger rebuild
