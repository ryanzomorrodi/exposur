#' Calculate Anderson et al. Precipitation
#'
#' @param daily_avgs Output from `calc_daily_avgs()`
#' @param closest_dist Output from `calc_closest_dist()`
#' @param days_before Number of days before closest distance (default 1)
#' @param days_after Number of days after closest distance (default 2)
#' 
#' @return Cumulative precipitation from `days_before` to `days_after`
#' the date of the closest distance for each geography
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_anderson_prcp <- calc_anderson_prcp(
#'   harvey_daily_avgs,
#'   harvey_closest_dist
#' )
#' }
calc_anderson_prcp <- function(
  daily_avgs,
  closest_dist,
  days_before = 1,
  days_after = 2
) {
  daily_avgs |>
    dplyr::left_join(closest_dist, by = "geoid") |>
    dplyr::group_by(.data$geoid) |>
    dplyr::filter(
      .data$date >= .data$closest_date - lubridate::days(days_before) &
      .data$date <= .data$closest_date + lubridate::days(days_after)
    ) |>
    dplyr::summarise("cum_prcp" = sum(.data$mean.prcp_prcp))
}

#' Calculate Anderson et al. Wind
#'
#' @param hurdat2 Output from `get_hurdat2()`
#' @param geography `sf` object for the area of interest
#' @param geoid Unique identifier for geography
#' 
#' @return Maximum wind and gust speed and duration calculated
#' using the `stormwindmodel`
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_anderson_wind <- calc_anderson_wind(
#'   harvey_hurdat2,
#'   TXLA_tracts
#' )
#' }
calc_anderson_wind <- function(
  hurdat2,
  geography,
  geoid = "GEOID"
) {
  hurdat2 <- hurdat2 |>
    dplyr::mutate(
      longitude = purrr::map_dbl(sf::st_geometry(hurdat2), 1),
      latitude = purrr::map_dbl(sf::st_geometry(hurdat2), 2),
      date = format(.data$datetime, "%Y%m%d%H%M"),
      wind = as.integer(.data$wind)
    ) |>
    dplyr::select(c("date", "longitude", "latitude", "wind")) |>
    sf::st_drop_geometry()

  geography <- geography |>
    sf::st_transform(4326) |>
    sf::st_centroid() |>
    dplyr::mutate(
      glon = purrr::map_dbl(sf::st_geometry(geography), 1),
      glat = purrr::map_dbl(sf::st_geometry(geography), 2),
      glandsea = TRUE
    ) |>
    dplyr::select(
      c(
        "gridid" = geoid,
        "glat",
        "glon",
        "glandsea"
      )
    ) |>
    sf::st_drop_geometry() |>
    tibble::tibble()

  get_grid_winds(hurdat2, geography) |>
    dplyr::mutate("date" = lubridate::as_datetime(.data$date)) |>
    dplyr::rename("geoid" = "gridid", "datetime" = "date")
}

## stormwindmodel fix
summarize_grid_winds_fix <- function(
  grid_winds,
  gust_duration_cut = 20,
  sust_duration_cut = 20,
  tint = 0.25
) {
  calc_sust_dur <- function(wind) {
    60 * tint * sum(wind > sust_duration_cut, na.rm = TRUE)
  }
  calc_gust_dur <- function(wind) {
    60 * tint * sum(wind > gust_duration_cut, na.rm = TRUE)
  }

  grid_wind_summary <- tibble::tibble(
    gridid = colnames(grid_winds),
    date = rownames(grid_winds)[apply(grid_winds, MARGIN = 2, FUN = which.max)],
    vmax_sust = apply(grid_winds, MARGIN = 2, FUN = max, na.rm = TRUE),
    vmax_gust = .data$vmax_sust * 1.49, 
    sust_dur = apply(grid_winds, MARGIN = 2, FUN = calc_sust_dur),
    gust_dur = apply(grid_winds, MARGIN = 2, FUN = calc_gust_dur)
  ) |>
    dplyr::mutate(date = ifelse(.data$vmax_sust == 0, NA, .data$date))

  return(grid_wind_summary)
}

## again, just a fix for the stormwindmodel
get_grid_winds <- function(
  hurr_track = stormwindmodel::floyd_tracks,
  grid_df = stormwindmodel::county_points,
  tint = 0.25,
  gust_duration_cut = 20,
  sust_duration_cut = 20,
  max_dist = 2222.4
) {
  grid_winds <- stormwindmodel::calc_grid_winds(
    hurr_track = hurr_track,
    grid_df = grid_df,
    tint = tint,
    max_dist = max_dist
  )
  grid_winds_summary <- summarize_grid_winds_fix(
    grid_winds = grid_winds$vmax_sust, 
    gust_duration_cut = gust_duration_cut, 
    sust_duration_cut = sust_duration_cut, 
    tint = tint
  )
    return(grid_winds_summary)
}