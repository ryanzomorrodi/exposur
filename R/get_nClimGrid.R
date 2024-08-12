nClimGrid_url <- "https://www.ncei.noaa.gov/data/nclimgrid-daily/access/grids/"

download_nClimGrid <- function(dates, path) {
  file_names <-
    stringr::str_c(
      "ncdd-",
      lubridate::year(dates),
      stringr::str_pad(lubridate::month(dates), 2, pad = "0"),
      "-grd-scaled.nc"
    )

  if (is.null(path)) {
    file_paths <- stringr::str_c(tempdir(), file_names, sep = "/")
  } else {
    file_paths <- stringr::str_c(path, file_names, sep = "/")
  }

  requests <- file_names[!file.exists(file_paths)] |>
    purrr::map(function(file_name) {
      httr2::request(nClimGrid_url) |>
        httr2::req_url_path_append(
          stringr::str_extract(file_name, "[:digit:]{4}")
        ) |>
        httr2::req_url_path_append(file_name)
    })

  resp <- requests |>
    httr2::req_perform_sequential(
      paths = file_paths[!file.exists(file_paths)],
      progress = TRUE
    )

  return(file_paths)
}

#' Download and Filter Weather Data
#'
#' Data is downloaded from NOAA [nClimGrid-Daily](https://www.ncei.noaa.gov/products/land-based-station/nclimgrid-daily)
#'
#' @param interval Dates to get weather data for
#' @param path Path to download files into
#'
#' @return `stars` raster of climate data over the interval specified
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_eaglei <- get_eaglei(
#'   interval = interval(harvey_start, harvey_end + days(5)),
#'   path = "raw-data/eaglei"
#' )
#' }
get_nClimGrid <- function(interval, path = NULL) {
  start_date <- lubridate::floor_date(lubridate::int_start(interval), "month")
  end_date <- lubridate::floor_date(lubridate::int_end(interval), "month")
  dates <- seq(start_date, end_date, by = "1 month")

  file_paths <- download_nClimGrid(dates, path)

  purrr::map(file_paths, stars::read_ncdf) |>
    do.call(what = c) |>
    dplyr::filter(.data$time %within% interval)
}
