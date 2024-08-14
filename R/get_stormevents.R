stormevents_url <- "https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

download_stormevents <- function(year, path) {
  stormevents_file <- rvest::read_html(stormevents_url) |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("\\.csv.gz$") |>
    stringr::str_subset("details") |>
    stringr::str_subset(stringr::str_c("d", year))
  
  if (!is.null(path)) {
    file_path <- stringr::str_c(path, stormevents_file, sep = "/")
  } else {
    file_path <- stringr::str_c(tempdir(), stormevents_file, sep = "/")
  }

  if (!file.exists(file_path)) {
    response <- httr2::request(stormevents_url) |>
      httr2::req_url_path_append(stormevents_file) |>
      httr2::req_perform(path = file_path)
  }

  return(file_path)
}

get_stormevents <- function(interval, path = NULL) {
  year <- lubridate::year(lubridate::int_start(interval))

  download_stormevents(year, path) |>
    readr::read_csv(
      col_types = readr::cols_only(
        "EPISODE_ID" = readr::col_character(),
        "STATE_FIPS" = readr::col_character(),
        "CZ_TYPE" = readr::col_character(),
        "CZ_FIPS" = readr::col_character(),
        "CZ_TIMEZONE" = readr::col_character(),
        "EVENT_TYPE" = readr::col_character(),
        "BEGIN_DATE_TIME" = readr::col_character(),
        "END_DATE_TIME" = readr::col_character(),
        "BEGIN_LAT" = readr::col_double(),
        "BEGIN_LON" = readr::col_double(),
        "END_LAT" = readr::col_double(),
        "END_LON" = readr::col_double()
      )
    ) |>
    dplyr::mutate(
      state_fips = stringr::str_pad(.data$STATE_FIPS, 2, "left", pad = "0"),
      cz_fips = stringr::str_pad(.data$CZ_FIPS, 3, "left", pad = "0"),
      geoid = stringr::str_c(state_fips, cz_fips),
      hours = lubridate::hours(
        stringr::str_extract(.data$CZ_TIMEZONE, "-(.*)")
      ),
      begin_datetime = as_datetime(
        .data$BEGIN_DATE_TIME, format = "%d-%b-%y %H:%M:%S"
      ) - hours,
      end_datetime = as_datetime(
        .data$END_DATE_TIME, format = "%d-%b-%y %H:%M:%S"
      ) - hours
    ) |>
    dplyr::select(
      c(
        "event_id" = "EPISODE_ID",
        "event_type" = "EVENT_TYPE",
        "cz_type" = "CZ_TYPE",
        "geoid",
        "begin_datetime",
        "end_datetime",
        "begin_lon" = "BEGIN_LON",
        "begin_lat" = "BEGIN_LAT",
        "end_lon" = "END_LON",
        "end_lat" = "END_LAT"
      )
    )
}