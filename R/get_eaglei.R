eaglei_url <- "https://figshare.com/ndownloader/articles/24237376/versions/2"

download_eaglei <- function(path) {
  if (is.null(path)) {
    file_path <- stringr::str_c(tempdir(), "eaglei.zip", sep = "/")
  } else {
    file_path <- stringr::str_c(path, "eaglei.zip", sep = "/")
  }

  if (!file.exists(file_path)) {
    cat("The eaglei zip file is 8+ GB, so this will take some time.\n")
    cat(stringr::str_c("Downloading to: ", file_path, "\n"))

    response <- httr2::request(eaglei_url) |>
      httr2::req_perform(path = file_path)
  }

  return(file_path)
}

#' Download and Filter Outage Data
#'
#' Data is downloaded from publically available data published
#' by [Brelsford et al.](https://www.nature.com/articles/s41597-024-03095-5)
#'
#' @param interval Dates to get outage data for
#' @param path Path to download files into
#'
#' @return Maximum precentage and average duration of outage
#' by county
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_eaglei <- get_eaglei(
#'   interval = interval(harvey_start, harvey_end + days(5)),
#'   path = "raw-data/eaglei"
#' )
#' }
get_eaglei <- function(interval, path = NULL) {
  file_path <- download_eaglei(path)

  year <- lubridate::year(lubridate::int_start(interval))

  mcc <- readr::read_csv(unz(file_path, "MCC.csv")) |>
    dplyr::rename("GEOID" = "County_FIPS") |>
    dplyr::mutate(
      "GEOID" = stringr::str_pad(.data$GEOID, 5, pad = "0"),
      "Customers" = as.integer(.data$Customers)
    )

  readr::read_csv(unz(
    file_path,
    stringr::str_c("eaglei_outages_", year, ".csv")
  )) |>
    dplyr::filter(.data$run_start_time %within% interval) |>
    dplyr::full_join(x = mcc, by = c("GEOID" = "fips_code")) |>
    dplyr::rename("customers" = .data$Customers)  |>
    dplyr::relocate("customers", .after = "customers_out")
}
