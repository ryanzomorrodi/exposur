hurdat_url <- "https://www.nhc.noaa.gov/data/hurdat/"

download_newest_hurdat2 <- function(path) {
  hurdat_newest <- rvest::read_html(stringr::str_c(hurdat_url, "?C=S;O=D")) |>
    rvest::html_nodes("a") |>
    rvest::html_attr("href") |>
    stringr::str_subset("\\.txt") |>
    purrr::pluck(1)


  if (is.null(path)) {
    file_path <- stringr::str_c(tempdir(), hurdat_newest, sep = "/")
  } else {
    file_path <- stringr::str_c(path, hurdat_newest, sep = "/")
  }

  if (!file.exists(file_path)) {
    response <- httr2::request(hurdat_url) |>
      httr2::req_url_path_append(hurdat_newest) |>
      httr2::req_perform(path = file_path)
  }

  return(file_path)
}

#' Download HURDAT2 Best Track
#'
#' Data is downloaded from NOAA [HURDAT2 HTTPS Server](https://www.nhc.noaa.gov/data/hurdat/)
#'
#' @param storm_id Storm name in all caps, dash, year (EX: "HARVEY-2017")
#' @param path Path to download files into
#'
#' @return HURDAT2 best track for a given storm
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_hurdat2  <- get_hurdat2(
#'   storm_id = "HARVEY-2017",
#'   path = "raw-data/hurdat2"
#' )
#' }
get_hurdat2 <- function(storm_id, path = NULL) {
  hurdat2 <- download_newest_hurdat2(path) |>
    readr::read_lines()

  hurdat2_names <- hurdat2[stringr::str_detect(hurdat2, "AL[0-9]{6}.")] |>
    stringr::str_c(collapse = "\n") |>
    readr::read_csv(
      col_names = c("usa_atcf_id", "storm_id", "n"),
      col_types = readr::cols_only(
        "usa_atcf_id" = readr::col_character(),
        "storm_id" = readr::col_character(),
        "n" = readr::col_integer()
      )
    )

  hurdat2_data <- hurdat2[!stringr::str_detect(hurdat2, "AL[0-9]{6}.")] |>
    stringr::str_c(collapse = "\n") |>
    readr::read_csv(
      col_names = c(
        "date", "time", "record_id", "status", "lat", "lon", "wind"
      ),
      col_types = readr::cols_only(
        "date" = readr::col_date(format = "%Y%m%d"),
        "time" = readr::col_time(format = "%H%M"),
        "record_id" = readr::col_character(),
        "status" = readr::col_character(),
        "lat" = readr::col_character(),
        "lon" = readr::col_character(),
        "wind" = readr::col_character()
      )
    ) |>
    dplyr::select(-c("record_id", "status"))

  dplyr::bind_cols(
    tidyr::uncount(hurdat2_names, n),
    hurdat2_data
  ) |>
    dplyr::mutate(
      "storm_id" = dplyr::if_else(
        .data$storm_id == "UNNAMED",
        "UNNAMED",
        stringr::str_c(.data$storm_id, lubridate::year(.data$date), sep = "-")
      )
    ) |>
    dplyr::filter(
      .data$storm_id == .env$storm_id
    ) |>
    dplyr::mutate(
      "lat_dir" = stringr::str_extract(.data$lat, "[N|S]$"),
      "lon_dir" = stringr::str_extract(.data$lon, "[E|W]$"),
      "lat" = as.numeric(stringr::str_remove(.data$lat, "[N|S]$")),
      "lon" = as.numeric(stringr::str_remove(.data$lon, "[E|W]$")),
      "lat" = ifelse(.data$lat_dir == "S", -1 * .data$lat, .data$lat),
      "lon" = ifelse(.data$lon_dir == "W", -1 * .data$lon, .data$lon)
    ) |>
    dplyr::mutate(
      "date" = lubridate::as_datetime(stringr::str_c(.data$date, .data$time))
    ) |>
    dplyr::select(-c("lat_dir", "lon_dir", "time")) |>
    dplyr::rename("datetime" = "date") |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326)
}
