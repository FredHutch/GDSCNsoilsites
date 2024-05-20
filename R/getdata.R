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
    rename("coord" = 4) %>%
    mutate(coord = str_replace_all(coord, "[//(,//)]*", "")) %>%
    tidyr::separate(coord,
                    into = c("latitude", "longitude"),
                    sep = " ") %>%
    mutate(longitude = case_when(
      as.numeric(longitude) > 0 ~ as.numeric(longitude) * -1,
      TRUE ~ as.numeric(longitude)
    )) %>%
    mutate(latitude = as.numeric(latitude))

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
  # Set filename and URLs
  google_sheet_url_1 <-
    "https://docs.google.com/spreadsheets/d/1KrPJe9OOGuix2EAvcTfuspAe3kqN-y20Huyf5ImBEl4/edit#gid=804798874"
  google_sheet_url_2 <-
    "https://docs.google.com/spreadsheets/d/1l7wuOt0DZp0NHMAriedbG4EJkl06Mh-G01CrVh8ySJE/edit#gid=804798874"
  google_sheet_url_3 <-
    "https://docs.google.com/spreadsheets/d/1duPtC8L9KL51YQzQ1rZ5785iH3RjABZFCYu8SWjAPTU/edit#gid=1458650276"
  google_sheet_url_4 <- # Soil data
    "https://docs.google.com/spreadsheets/d/109xYUM48rjj33B76hZ3bNlrm8u-_S6uyoE_3wSCp0r0/edit#gid=1458650276"
  soil_file <- "soil_data.csv"

  # Check if the file is/has been written.
  # If not, read it in from google sheets, save as `soil_data`
  if (!(file.exists(soil_file))) {
    do_gs4_auth()
    soil_data <-
      inner_join(
        read_sheet(google_sheet_url_1),
        read_sheet(google_sheet_url_2),
        by = c(`What is your site name?` = 'site_name'),
        keep = TRUE
      ) %>%
      inner_join(read_sheet(google_sheet_url_3), by = "full_name") %>%
      inner_join(
        read_sheet(google_sheet_url_4, col_types = "ccccnnnnnnnnnnnnnnnnnnnnnnn"),
        by = c("full_name", "site_id")
      ) %>% # Special characters
      rename("type" = `Which best describes your site?`) %>%
      separate("type", into = c("type", "type2"), sep = ":")
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
    dplyr::mutate(url = paste0(
      'https://lh3.googleusercontent.com/d/',
      google_img_id
    )) %>%
    select(url) %>%
    pull()

  # HEAVY METALS
  # Get lead values
  lead <-
    soil_data %>%
    select(Pb_EPA3051) %>%
    pull()

  lead <- cut(
    lead,
    c(0, 25, 50, 75, 100, 500),
    include.lowest = T,
    labels = c('<25', '25-50', '50-75', '75-100', '100+')
  )

  # Get arsenic values
  arsenic <-
    soil_data %>%
    select(As_EPA3051) %>%
    mutate(As_EPA3051 = case_when(As_EPA3051 == "< 3.0" ~ "3.0", TRUE ~ As_EPA3051)) %>%
    drop_na(As_EPA3051) %>%
    mutate(As_EPA3051 = as.numeric(As_EPA3051)) %>%
    pull()

  arsenic <- cut(
    arsenic,
    c(0, 3.0, 5, 10, 15, 1000),
    include.lowest = T,
    labels = c('< 3.0', '3.0-5.0', '5.0-10.0', '10.0-15.0', '15.0+')
  )

  # Get iron values
  iron <-
    soil_data %>%
    select(Fe_Mehlich3) %>%
    pull()

  iron <- cut(
    iron,
    c(0, 100, 150, 200, 250, 1000),
    include.lowest = T,
    labels = c('<100', '100-150', '150-200', '200-250', '250+')
  )

  return(list(
    points = gps_points,
    sitenames = sitenames,
    image_urls = image_urls,
    lead = lead,
    arsenic = arsenic,
    iron = iron
  ))
}


#' Produce data in a nice clean format for browsing on the app.
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' get_browseable_data()
get_browseable_data <- function() {
  soil_data_to_browse <-
    clean_gps_points(getdata())

  soil_data_to_browse <-
    soil_data_to_browse %>%
    mutate(timestamp = lubridate::as_datetime(Timestamp.x)) %>%
    mutate(date_sampled = lubridate::date(timestamp)) %>%
    select(site_id, site_name, type, date_sampled, latitude, longitude)

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

# Comment here to trigger rebuild
