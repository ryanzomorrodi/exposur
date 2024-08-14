#' Calculate Closest Distance and Time
#'
#' @description
#' Utilizes [`stormwindmodel`](https://github.com/geanders/stormwindmodel)
#' to impute the `hurdat2` track to 15 minute intervals, calculate
#' at what time point the storm is closest, and record the distance
#' from track at that time.
#'
#' @details
#' ## Centroids
#' By default this package will take the mean centroid of whatever
#' geography provided to it. If you would like to approximate exposure
#' at the population weighted centroid use `centr::mean_center()` on
#' your geography before providing it to this function.
#'
#' @param hurdat2 Output from `get_hurdat2()`
#' @param geography `sf` object for the area of interest
#' @param geoid Unique identifier for geography
#'
#' @return Closest distance and time to track for each geography
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_closest_dist <- calc_closest_dist(
#'   harvey_hurdat2,
#'   TXLA_tracts
#' )
#' }
calc_closest_dist <- function(hurdat2, geography, geoid = "GEOID") {
  geography <- sf::st_centroid(geography) |>
    sf::st_transform(4326)

  interp_track <- calc_interpolated_track(hurdat2)

  geography |>
    dplyr::mutate(
      "near_idx" = sf::st_nearest_feature(geography, interp_track),
      "closest_date" = interp_track$datetime[.data$near_idx],
      "closest_dist" = sf::st_distance(
        interp_track$geometry[.data$near_idx],
        geography,
        by_element = TRUE
      )
    ) |>
    sf::st_drop_geometry() |>
    tibble::tibble() |>
    dplyr::select(c("geoid" = "GEOID", "closest_dist", "closest_date"))
}

calc_interpolated_track <- function(track) {
  track <- track |>
    dplyr::mutate(
      lon = purrr::map_dbl(sf::st_geometry(track), 1),
      lat = purrr::map_dbl(sf::st_geometry(track), 2)
    )

  dif <- difftime(
    track$datetime,
    min(track$datetime),
    units = "hour"
  ) |>
    as.integer()
  seq_int <- seq(0, max(dif), 0.25)
  seq_date <- seq(
    min(track$datetime),
    max(track$datetime),
    "15 min"
  )

  tibble::tibble(
    datetime = seq_date,
    lon = stormwindmodel::interpolate_spline(dif, track$lon, seq_int),
    lat = stormwindmodel::interpolate_spline(dif, track$lat, seq_int)
  ) |>
    sf::st_as_sf(coords = c("lon", "lat"), crs = 4326, na.fail = FALSE)
}
