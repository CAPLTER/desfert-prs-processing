#' @title run the PRS import workflow
#'
#' @description `run_prs_load` reads and formats a WesternAg PRS Excel file,
#' validates it against the database, and inserts it into
#' `urbancndep.prs_analysis` in a single transaction. With `dry_run = TRUE` the
#' insert is executed and then rolled back. Stops on any failure.
#'
#' @export

run_prs_load <- function(input, host = "localhost", dbname = "caplter", user, port = 5432, dry_run = FALSE) {

  cat(sprintf("%s run: %s\n", if (dry_run) "DRY" else "LIVE", basename(input)))
  cat(sprintf("database: %s@%s:%s/%s\n", user, host, port, dbname))

  prs <- read_prs_file(input)

  con <- connect_pg(host, dbname, user, port)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  new_prs <- format_prs_data(prs, additional_known_analytes = db_analytes(con))

  validate_plots(con, new_prs$plot_id)

  n <- load_prs(con, new_prs, dry_run = dry_run)

  cat(sprintf(
    paste0(
      "samples: %d (WAL %d-%d)\n",
      "analytes: %s\n",
      "deployed: %s to %s\n",
      "below detection limit: %d of %d values\n",
      "rows %s: %d\n"
    ),
    dplyr::n_distinct(new_prs$wal_id), min(new_prs$wal_id), max(new_prs$wal_id),
    paste(unique(new_prs$analyte), collapse = ", "),
    min(new_prs$start_date, na.rm = TRUE), max(new_prs$end_date, na.rm = TRUE),
    sum(!is.na(new_prs$flag)), nrow(new_prs),
    if (dry_run) "that would be inserted (rolled back)" else "inserted", n
  ))

  invisible(new_prs)

}
