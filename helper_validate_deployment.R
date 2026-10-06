#' @title check for reasonable (< 180 d) deployment period
#'
#' @description `validate_deployment_times` checks to ensure that reported
#' deployment dates are reasonable (greater than 0 and less than 180 days). The
#' purpose of this validation is to help identify deployment dates that may have
#' been entered incorrectly. Stops on failure.
#'
#' @param start_date,end_date Date vectors (burial and retrieval).
#'
#' @export

validate_deployment_times <- function(start_date, end_date, sample_id) {

  deployment_days <- as.numeric(end_date - start_date)

  bad <- !is.na(deployment_days) & (deployment_days <= 0 | deployment_days >= 180)

  if (any(bad)) {
    stop(
      "unreasonable deployment period (must be > 0 and < 180 d) for sample(s): ",
      paste0(sample_id[bad], " (", deployment_days[bad], " d)", collapse = ", ")
    )
  }

}
