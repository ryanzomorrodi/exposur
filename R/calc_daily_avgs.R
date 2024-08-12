#' Calculate Daily Average for Each Geography
#'
#' @param nClimGrid Output from `get_nClimGrid()`
#' @param geography `sf` object for the area of interest
#' @param geoid Unique identifier for geography
#'
#' @return Daily average for all `nClimGrid` measures for each
#' geography
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_daily_avgs <- calc_daily_avgs(
#'   harvey_nClimGrid,
#'   TXLA_tracts
#' )
#' }
calc_daily_avgs <- function(nClimGrid, geography, geoid = "GEOID") {
  cat("Transforming to WGS 1984...\n")

  geography <- sf::st_transform(geography, 4326)
  nClimGrid <- stars::st_warp(nClimGrid, crs = 4326)

  num_days <- stars::st_dimensions(nClimGrid) |>
    purrr::pluck(3, "to")
  start_date <- stars::st_dimensions(nClimGrid) |>
    purrr::pluck(3, "offset")
  end_date <- start_date + lubridate::days(num_days)
  date_seq <- seq(start_date, end_date, by = "1 day")

  purrr::map(
    1:num_days,
    function(day) {
      cat(stringr::str_c(date_seq[day], "\n"))

      day_rast <- nClimGrid |>
        dplyr::slice("time", day) |>
        terra::rast()

      exactextractr::exact_extract(day_rast, geography, "mean") |>
        tibble::tibble() |>
        dplyr::mutate(
          "date" = lubridate::as_date(date_seq[day]),
          "geoid" = purrr::pluck(geography, geoid)
        )
    }
  ) |>
    dplyr::bind_rows() |>
    dplyr:: relocate(c("geoid", "date"), .before = dplyr::everything())

}
