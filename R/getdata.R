
#' Scrubs SITE data according to faculty requests. This is an interactive process that
#' could likely be automated using a GitHub Action Google Sheet pull at some point.
#'
#' @param infile Character string specifying the path to the input CSV file containing raw data
#' @param outfile Character string specifying the path where the scrubbed data will be saved
#'
#' @return A `data.frame` containing the processed data with:
#'   - Filtered rows based on public sharing permissions
#'   - Masked GPS measurements
#'   - Preserved data structure and column names
#'
#' @examples
#' scrubsitedata(infile = "BioDIGS Sample Data and Kit Request MASTER - ALL_SAMPLES - SITE DATA.csv", outfile = "data/snapshots/BioDIGS_sites_20250616.csv")
scrubsitedata <- function(infile, outfile) {
  scrubbed_data <-
    read.csv(infile) %>%
    # Remove records not cleared for public sharing
    filter(public_ok != "not yet provided") %>%
    # Mask GPS for records without GPS approval
    mutate(gps = case_when(str_detect(public_ok, "NO GPS") ~ NA, TRUE ~ gps)) %>%
    mutate(across(gps |
                    mgmt_type | google_img_id, ~ na_if(., "not yet provided"))) %>%
    # Remove Tuba City names for anonymity
    mutate(site_name = case_when(str_detect(site_id, "TC0") ~ NA, TRUE ~ site_name))

  # Save processed data
  write.csv(scrubbed_data, outfile)

  # Notify if overwriting existing file
  if (file.exists(outfile)) {
    message(paste0("Overwriting existing file ", outfile))
  }
}


#' Scrubs SOIL data according to faculty requests. This is an interactive process that
#' could likely be automated using a GitHub Action Google Sheet pull at some point.
#'
#' @param infile Character string specifying the path to the input CSV file containing raw data
#' @param outfile Character string specifying the path where the scrubbed data will be saved
#'
#' @return A `data.frame` containing the processed data with:
#'   - Filtered rows based on public sharing permissions
#'   - Masked soil property measurements
#'   - Preserved data structure and column names
#'
#' @examples
#' scrubsoildata(infile = "BioDIGS Sample Data and Kit Request MASTER - ALL_SAMPLES - SOIL SAMPLES.csv", outfile = "data/snapshots/BioDIGS_soil_20250616.csv")
scrubsoildata <- function(infile, outfile) {
  scrubbed_data <-
    read.csv(infile) %>%
    # Remove records not cleared for public sharing
    filter(public_ok != "not yet provided") %>%
    # Mask soil measurements for NO SOIL DATA records (as NA)
    mutate(
      across(
        ends_with("EPA3051") |
          ends_with("Mehlich3") |
          water_pH |
          A.E_Buffer_pH |
          OM_by_LOI_pct | Est_CEC | Base_Sat_pct | P_Sat_ratio ,
        ~ case_when(str_detect(public_ok, "NO SOIL DATA") ~ NA, TRUE ~ .)
      )
    ) %>%
    mutate(across(collection_date |
                    team, ~ na_if(., "not yet provided"))) %>%
    # Remove Tuba City names for anonymity
    mutate(site_name_rep_detail = case_when(str_detect(site_id, "TC0") ~ NA, TRUE ~ site_name_rep_detail))

  # Save processed data
  write.csv(scrubbed_data, outfile)

  # Notify if overwriting existing file
  if (file.exists(outfile)) {
    message(paste0("Overwriting existing file ", outfile))
  }
}


#' Scrubs SEQUENCING metadata according to faculty requests. This is an interactive process that
#' could likely be automated using a GitHub Action Google Sheet pull at some point.
#'
#' @param infile Character string specifying the path to the input CSV file containing raw data
#' @param outfile Character string specifying the path where the scrubbed data will be saved
#'
#' @return A `data.frame` containing the processed data with:
#'   - Filtered rows based on public sharing permissions
#'   - Preserved data structure and column names
#'
#' @examples
#' scrubseqdata(infile = "BioDIGS Sample Data and Kit Request MASTER - ALL_SAMPLES - SEQ SAMPLES.csv", outfile = "data/snapshots/BioDIGS_seq_20250616.csv")
scrubseqdata <- function(infile, outfile) {
  scrubbed_data <-
    read.csv(infile) %>%
    # Remove records not cleared for public sharing; Mask any not planned for sequencing
    filter(public_ok != "not yet provided", date_sent_seq != FALSE) %>%
    mutate(across(collection_date |
                    team, ~ na_if(., "not yet provided"))) %>%
    # Remove Tuba City names for anonymity
    mutate(site_name_rep_detail = case_when(str_detect(site_id, "TC0") ~ NA, TRUE ~ site_name_rep_detail))

  # Save processed data
  write.csv(scrubbed_data, outfile)

  # Notify if overwriting existing file
  if (file.exists(outfile)) {
    message(paste0("Overwriting existing file ", outfile))
  }
}


#' #' Scrubs data according to faculty requests. This is an interactive process that
#' #' could likely be automated using a GitHub Action Google Sheet pull at some point.
#' #'
#' #' @param infile Character string specifying the path to the input CSV file containing raw data
#' #' @param outfile Character string specifying the path where the scrubbed data will be saved
#' #'
#' #' @return A `data.frame` containing the processed data with:
#' #'   - Filtered rows based on public sharing permissions
#' #'   - Masked EPA3051, Mehlich3, pH, buffer pH, OM_by_LOI_pct, and GPS measurements
#' #'   - Preserved data structure and column names
#' #'
#' #' @examples
#' #' scrubdata(infile = "BioDIGS Sample Data and Kit Request MASTER - ALL_SAMPLES.csv", outfile = "data/snapshots/BioDIGS_20250514.csv")
#' scrubdata <- function(infile, outfile) {
#'   scrubbed_data <-
#'     read.csv(infile) %>%
#'     # Remove records not cleared for public sharing
#'     filter(public_ok != "not yet provided") %>%
#'     # Mask soil measurements for NO SOIL DATA records (as NA)
#'     mutate(
#'       across(
#'         ends_with("EPA3051") |
#'           ends_with("Mehlich3") |
#'           water_pH | A.E_Buffer_pH | OM_by_LOI_pct,
#'         ~ case_when(public_ok == "GPS OK ; NO SOIL DATA" ~ NA, TRUE ~ .)
#'       )
#'     )  %>%
#'     # Mask all measurements including GPS for records without GPS approval
#'     mutate(
#'       across(
#'         ends_with("EPA3051") |
#'           ends_with("Mehlich3") |
#'           water_pH | A.E_Buffer_pH | OM_by_LOI_pct | gps,
#'         ~ case_when(public_ok == "NO GPS ; NO SOIL DATA" ~ NA, TRUE ~ .)
#'       )
#'     )
#'
#'   # Save processed data
#'   write.csv(scrubbed_data, outfile)
#'
#'   # Notify if overwriting existing file
#'   if (file.exists(outfile)) {
#'     message(paste0("Overwriting existing file ", outfile))
#'   }
#' }


#' Loads soil data from snapshot with local caching.
#'
#' @param measure Which dataset? Can be `sites`, `soil`, or `seq` data.
#' @return Soil data `data.frame`
#'
#' @export
#' @examples
#' getdata()
#' getdata(dataset = "soil")
getdata <- function(dataset = "sites") {

  if(dataset == "sites") {
    snapshot_path <- "data/snapshots/BioDIGS_sites_20250616.csv"
  } else if (dataset == "soil") {
    snapshot_path <- "data/snapshots/BioDIGS_soil_20250616.csv"
  } else {
    snapshot_path <- "data/snapshots/BioDIGS_seq_20250616.csv"
  }

  if (file.exists(snapshot_path)) {
    the_data <- read.csv(snapshot_path)[, -1]  # Remove first column
    return(the_data)
  }
}


#' #' Cleans data to make GPS coordinates consistent.
#' #'
#' #' @param the_data
#' #'
#' #' @return Cleaned `data.frame`
#' #' @export
#' #'
#' #' @examples
#' #' clean_gps_points(getdata())
#' clean_gps_points <- function(the_data) {
#'   # Clean up GPS points:
#'   # Remove parentheses, split column, fix negatives, and make numeric
#'   the_data <- the_data %>%
#'     filter(gps != "Not yet provided", gps != "not yet provided") %>%
#'     mutate(gps = str_replace_all(gps, "[//(,//)]*", "")) %>%
#'     tidyr::separate(gps,
#'                     into = c("latitude", "longitude"),
#'                     sep = " ") %>%
#'     mutate(longitude = case_when(
#'       as.numeric(longitude) > 0 ~ as.numeric(longitude) * -1,
#'       TRUE ~ as.numeric(longitude)
#'     )) %>%
#'     mutate(latitude = as.numeric(latitude))
#'
#'   return(the_data)
#' }
#'
#'
#' #' Clean the site name details in case we don't want "rep 1" or "rep 2" included
#' #'
#' #' @param the_data
#' #'
#' #' @return Cleaned `data.frame`
#' #' @export
#' #'
#' #' @examples clean_site_name_rep_detail(getdata())
#' clean_site_name_rep_detail <- function(the_data) {
#'   # Clean up site_name_rep_detail column
#'   # Keep only unique sites, and clean site details so no rep information is included.
#'   the_data <-
#'     the_data[match(unique(the_data$site_id), the_data$site_id), ] %>%
#'     mutate(site_name_rep_detail = str_remove(site_name_rep_detail, "( rep 1| Rep 1| rep #1| Rep #1)$")) %>%
#'     mutate(site_name_rep_detail = str_remove(site_name_rep_detail, "( rep 2| Rep 2| rep #2| Rep #2)$"))
#'
#'   return(the_data)
#' }


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

  # Extract faculty names
  partner_faculty <-
    soil_data %>%
    clean_site_name_rep_detail() %>%
    select(partner_faculty) %>%
    pull()

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
    partner_faculty = partner_faculty,
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
  site_data_to_browse <-
    getdata(dataset = "sites") %>%
    mutate(gps = str_replace_all(gps, "[//(,//)]*", "")) %>%
    tidyr::separate(gps,
                    into = c("latitude", "longitude"),
                    sep = " ") %>%
    mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude)) %>%
    select(!google_img_id)

  return(site_data_to_browse)
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
    getdata(dataset = "soil") %>%
    relocate(public_ok, .after = P_Sat_ratio) %>%
    mutate(date_sent_soil_analysis = na_if(date_sent_soil_analysis, "FALSE")) %>%
    mutate(
      across(
        collection_date | date_arrival_at_jhu | date_sent_soil_analysis,
        ~ lubridate::mdy(.)
      )
    )

  return(testing_data_to_browse)
}


#' Produce data in a nice clean format for browsing & downloading
#'
#' @return a `data.frame`
#' @export
#'
#' @examples
#' get_browseable_seq_data()
get_browseable_seq_data <- function() {
  seq_data_to_browse <-
    getdata(dataset = "seq") %>%
    relocate(public_ok, .after = Note) %>%
    mutate(date_sent_seq = na_if(date_sent_seq, "Planned")) %>%
    mutate(seq_date = na_if(seq_date, "not yet provided")) %>%
    mutate(
      across(
        collection_date | date_arrival_at_jhu | date_sent_seq | seq_date,
        ~ lubridate::mdy(.)
      )
    ) %>%
    select(!c(size_GB, reads))

  return(seq_data_to_browse)
}

# Comment here to trigger rebuild
