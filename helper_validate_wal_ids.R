#' @title confirm that all samples are included in the formatted data
#'
#' @description `validate_wal_ids` checks that the WAL ids in the imported
#' samples are the same as those in the formatted data, i.e., that no samples
#' were inappropriately filtered or otherwise excluded. Stops on failure.
#'
#' @export

validate_wal_ids <- function(imported_wal_ids, formatted_wal_ids) {

  if (!identical(sort(unique(imported_wal_ids)), sort(unique(formatted_wal_ids)))) {
    stop("wal ids of imported and formatted data do not match")
  }

}
