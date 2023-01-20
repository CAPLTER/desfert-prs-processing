#' @title format imported prs data for upload to database
#'
#' @description `format_prs_data` performs a number of steps to format PRS data
#' imported from Excel for upload to the database. Processing include data
#' validation (only deployment times as of this version).
#'
#' @export

format_prs_data <- function(imported_data) {

  validate_deployment_times(imported_data)

  formatted_prs <- imported_data |>
  dplyr::filter(!is.na(`Sample ID`)) |>
  dplyr::rename(`Total-N` = `Total N`) |>
  tidyr::pivot_longer(
    cols      = `Total-N`:last_col(),
    names_to  = "id",
    values_to = "result"
    ) |>
  # gather(id, result, `Total-N`:last_col()) |> # stack
  dplyr::mutate(
    plotid = as.numeric(gsub("[[:alpha:]]", "", `Sample ID`)),
    location = NA_character_,
    location = dplyr::case_when(
      grepl("a", `Sample ID`, ignore.case = TRUE) ~ "under plant",
      grepl("b", `Sample ID`, ignore.case = TRUE) ~ "between plant",
      TRUE ~ location
      ),
    location = dplyr::case_when(
      plotid > 75 ~ "BLANK",
      TRUE ~ location
      ),
    flag = dplyr::case_when(
      result <= 2.0 & id %in% two ~ "below detection limit",
      result <= 4.0 & id %in% four ~ "below detection limit",
      result <= 0.2 & id %in% two_tenths ~ "below detection limit",
      result <= 0.4 & id %in% four_tenths ~ "below detection limit",
      TRUE ~ NA_character_
    )
    ) |>
  pointblank::col_vals_make_set(
    columns = vars(location),
    set     = c("under plant", "between plant", "BLANK"),
    actions = pointblank::warn_on_fail()
  )

  validate_wal_ids(
    imported_prs_data  = imported_data,
    formatted_prs_data = formatted_prs
  )

  return(formatted_prs)

}
