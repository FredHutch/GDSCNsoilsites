#' Does Google Sheets authentication using a Google Service Account. It looks
#' for a `.json` service account key in the ".secrets" directory.
#'
#' @return Provides authentication analagous to gs4_auth()
#' @export
#'
#' @examples
#' # Run before read_sheet() functions
#' do_gs4_auth()
do_gs4_auth <- function() {
  gs4_auth(
    token = gargle::credentials_service_account(path = paste0(
      ".secrets/", grep(".json$", list.files(".secrets"), value = TRUE)
    ),
    scopes = "https://www.googleapis.com/auth/spreadsheets")
  )
}


#' Cleans data from Google Sheets to make GPS coordinates consistent.
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
#' @examples
clean_site_name_rep_detail <- function(the_data) {
  # Clean up site_name_rep_detail column
  # Keep only unique sites, and clean site details so no rep information is included.
  the_data <-
    the_data[match(unique(the_data$site_id), the_data$site_id), ] %>%
    mutate(site_name_rep_detail = str_remove(site_name_rep_detail, "( rep 1| Rep 1| rep #1| Rep #1)$")) %>%
    mutate(site_name_rep_detail = str_remove(site_name_rep_detail, "( rep 2| Rep 2| rep #2| Rep #2)$"))

  return(the_data)
}


#' Gets relevant data from Google Sheets.
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
  # Read in from Google, clean GPS points
  soil_data <- clean_gps_points(getdata())

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


#' Produce DNA concentration data in a clean format for browsing on the app.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' get_dna_conc_data()
get_dna_conc_data <- function() {
  # Read in from Google, clean GPS points
  dna_data <- getdata()

  dna_data_to_browse <-
    dna_data %>%
    select(site_id,
           site_name,
           ul_hydration,
           qubit_concentration_ng_ul,
           total_ng,
           type)

  return(dna_data_to_browse)
}


#' Get and write soil spatial data so that soil type areas can be plotted on a
#' map. These are analagous to GIS polygons.
#'
#' @return an object of class SpatialPolygonsDataFrame and writes rds.
#' @export
#'
#' @examples
#' make_soil_data()
make_soil_data <- function() {
  # https://data.imap.maryland.gov/datasets/9c48f92b2b4e4663aa78fdd64a1ab010
  soil_type_data_file <- "data/soil_types/soil_type_data.rds"

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

  saveRDS(soil_type_data, file = soil_type_data_file)

  return(soil_type_data)
}


#' Get soil spatial data so that soil type areas can be plotted on a
#' map. These are analagous to GIS polygons. This function does some checking
#' to see that the `soil_type_data.rds` file is younger than other data files.
#'
#' @return an object of class SpatialPolygonsDataFrame
#' @export
#'
#' @examples
#' get_soil_data()
get_soil_data <- function() {
  # https://data.imap.maryland.gov/datasets/9c48f92b2b4e4663aa78fdd64a1ab010

  soil_type_data_file <- "data/soil_types/soil_type_data.rds"
  file_info <-
    dplyr::bind_rows(lapply(paste0(
      "data/soil_types/", list.files("data/soil_types/")
    ),
    file.info))

  # If there are data files younger than the composite file, remake it.
  if (file.exists(soil_type_data_file)) {
    if (any(file_info$mtime > file_info[soil_type_data_file,]$mtime)) {
      soil_type_data <- make_soil_data()
    } else {
      soil_type_data <- readRDS(soil_type_data_file)
    }
  } else {
    soil_type_data <- make_soil_data()
  }

  return(soil_type_data)
}


#' Produce soil testing data in a nice clean format for browsing on the app.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' get_browseable_testing_data()
get_browseable_testing_data <- function() {
  testing_data_to_browse <- getdata()

  testing_data_to_browse <-
    testing_data_to_browse %>%
    select(
      site_id,
      site_name,
      type,
      tidyr::ends_with("EPA3051"),
      water_pH,
      OM_by_LOI_pct,
      tidyr::ends_with("Mehlich3"),
      Est_CEC,
      Base_Sat_pct,
      P_Sat_ratio
    ) %>%
    mutate(As_EPA3051 = as.numeric(case_when(As_EPA3051 == "< 3.0" ~ "0",
                                             TRUE ~ As_EPA3051))) %>%      # As can't be detected lower than 3.0
    mutate(Cd_EPA3051 = as.numeric(case_when(Cd_EPA3051 == "< 0.2" ~ "0",
                                             TRUE ~ Cd_EPA3051))) %>%      # Cd can't be detected lower than 0.2
    mutate(
      region = case_when(
        startsWith(site_id, "M") ~ "Montgomery County",
        startsWith(site_id, "B") ~ "Baltimore City",
        startsWith(site_id, "S") ~ "Seattle"
      )
    )

  return(testing_data_to_browse)
}
