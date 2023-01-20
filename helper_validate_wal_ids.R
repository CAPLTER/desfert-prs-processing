#' @title confirm that all samples are included in the formatted data
#'
#' @description `validate_wal_ids` checks that the `WAL #` ids in the
#' imported data are also included in the formatted data.
#'
#' @note Owing to the unfriendly column naming that WesternAg uses, this
#' function employs indexing to isolate the `WAL #` column(s) in the
#' uploaded and formatted data.
#'
#' @export

validate_wal_ids <- function(imported_prs_data, formatted_prs_data) {

  wal_ids_upload <- suppressWarnings(as.integer(imported_prs_data[[1]]))
  wal_ids_upload <- sort(wal_ids_upload[!is.na(wal_ids_upload)])

  wal_ids_format <- suppressWarnings(as.integer(formatted_prs_data[[1]]))
  wal_ids_format <- sort(unique(wal_ids_format))

  if (!identical(wal_ids_upload, wal_ids_format)) {

    stop("wal ids of imported and formatted data do not match")

  }

}
