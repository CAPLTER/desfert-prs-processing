#' @title check for reasonable (< 180 d) deployment period
#'
#' @description `validate_deployment_times` checks to ensure that reported
#' deployment dates are reasonable (less than 180 days; greater than 0 days).
#' The purpose of this validation is to help identify deployment dates that may
#' have been entered incorrectly.
#'
#' @export

validate_deployment_times <- function(prs_data) {

  prs_data |>
    pointblank::col_vals_lt(
      columns       = vars(date_range),
      value         = 180,
      na_pass       = TRUE,
      preconditions = function(x) { dplyr::mutate(x, date_range = lubridate::ymd(x$`Retrieval Date`) - lubridate::ymd(x$`Burial Date`))},
      actions       = pointblank::stop_on_fail()
    )

  prs_data |>
    pointblank::col_vals_gt(
      columns       = vars(date_range),
      value         = 0,
      na_pass       = TRUE,
      preconditions = function(x) { dplyr::mutate(x, date_range = lubridate::ymd(x$`Retrieval Date`) - lubridate::ymd(x$`Burial Date`))},
      actions       = pointblank::stop_on_fail()
    )

}
