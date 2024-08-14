#' Calculate Outage Max Precentage and Average Duration
#'
#' @description
#' Estimates the maximum outage precentage and average
#' duration of outage for each county.
#'
#' @param eaglei Output from `get_outage()`
#' @param state_fips state fips code to restrict output to
#'
#' @return Maximum precentage and average duration of outage
#' by county
#' @export
#'
#' @examples
#' \dontrun{
#' harvey_outage <- calc_outage(
#'   harvey_eaglei,
#'   TXLA_fips
#' )
#' }
calc_outage <- function(eaglei, state_fips = NULL) {
  output <- eaglei |>
    dplyr::group_by(.data$geoid) |>
    dplyr::mutate(
      "customers_out" = dplyr::if_else(
        is.na(.data$customers_out),
        0,
        .data$customers_out
      ),
      "customers" = dplyr::if_else(is.na(.data$customers), 0, .data$customers),
      "pct_out" = dplyr::if_else(
        .data$customers_out / .data$customers > 1,
        1,
        .data$customers_out / .data$customers
      )
    ) |>
    dplyr::summarise(
      "max_pct_out" = max(.data$pct_out),
      "avg_customer_hrs_out" = sum(.data$pct_out) * 0.25
    )

  if (!is.null(state_fips)) {
    output <- output |>
      filter(stringr::str_sub(.data$geoid, 1, 2) %in% state_fips)
  }

  output
}
