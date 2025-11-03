
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
    # Mask zip codes without GPS approval
    mutate(closest_zip = case_when(str_detect(public_ok, "NO GPS") ~ NA, TRUE ~ closest_zip)) %>%
    mutate(across(gps |
                    mgmt_type | google_img_id, ~ na_if(., "not yet provided"))) %>%
    # Remove Tuba City names for anonymity
    mutate(site_name = case_when(str_detect(site_id, "TC0") ~ NA, TRUE ~ site_name))

  # Notify if overwriting existing file
  if (file.exists(outfile)) {
    message(paste0("Overwriting existing file ", outfile))
  }

  # Save processed data
  write.csv(scrubbed_data, outfile)
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

  # Notify if overwriting existing file
  if (file.exists(outfile)) {
    message(paste0("Overwriting existing file ", outfile))
  }

  # Save processed data
  write.csv(scrubbed_data, outfile)
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

  # Notify if overwriting existing file
  if (file.exists(outfile)) {
    message(paste0("Overwriting existing file ", outfile))
  }

  # Save processed data
  write.csv(scrubbed_data, outfile)
}


#' Loads soil data from snapshot with local caching.
#'
#' @param measure Which dataset? Can be `sites`, `soil`, or `seq` data.
#' @return Soil data `data.frame`
#'
#' @export
#' @examples
#' getdata()
#' getdata(dataset = "soil")
getdata <- function(dataset = "sites", snapshot = "20251103") {

  if(dataset == "sites") {
    snapshot_path <- paste0("data/snapshots/BioDIGS_sites_", snapshot,".csv")
  } else if (dataset == "soil") {
    snapshot_path <- paste0("data/snapshots/BioDIGS_soil_", snapshot,".csv")
  } else {
    snapshot_path <- paste0("data/snapshots/BioDIGS_seq_", snapshot,".csv")
  }

  if (file.exists(snapshot_path)) {
    the_data <- read.csv(snapshot_path)[, -1]  # Remove first column
    return(the_data)
  }
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
    relocate(Note, .before = public_ok) %>%
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


#' Partition out the data into a list for quick use in plots.
#'
#' @return a `list` of gps coordinates, site names, faculty partner names,
#' and image urls
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
  # Extract gps coordinates
  gps_points <-
    get_browseable_site_data() %>%
    select(longitude, latitude) %>%
    as.data.frame()

  # Extract faculty names
  partner_faculty <-
    get_browseable_site_data() %>%
    select(partner_faculty) %>%
    pull()

  # Pull out metadata site names separately
  sitenames <-
    get_browseable_site_data() %>%
    select(site_name) %>%
    pull()

  # Adjust image urls
  image_urls <-
    getdata() %>%
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
