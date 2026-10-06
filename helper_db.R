#' @title database helpers for loading PRS data
#'
#' @description Connection, validation, and loading of formatted PRS data to
#' `urbancndep.prs_analysis`. The password is never handled here: it is read by
#' libpq from `PGPASSWORD` or `~/.pgpass`.

prs_schema <- "urbancndep"
prs_table  <- "prs_analysis"

connect_pg <- function(host, dbname, user, port = 5432) {

  DBI::dbConnect(
    drv      = RPostgres::Postgres(),
    host     = host,
    dbname   = dbname,
    user     = user,
    port     = port
  )

}

#' analytes already present in the database (excluding the legacy `Total N`
#' label that was used before names were standardized)

db_analytes <- function(con) {

  analytes <- DBI::dbGetQuery(
    con,
    paste0("SELECT DISTINCT analyte FROM ", prs_schema, ".", prs_table)
  )$analyte

  setdiff(analytes, "Total N")

}

validate_plots <- function(con, plot_ids) {

  existing <- DBI::dbGetQuery(con, paste0("SELECT id FROM ", prs_schema, ".plots"))$id
  missing  <- setdiff(unique(plot_ids), existing)

  if (length(missing) > 0) {
    stop("plot id(s) not found in ", prs_schema, ".plots: ", paste(sort(missing), collapse = ", "))
  }

}

#' insert formatted data in a single transaction; roll back (rather than
#' commit) when `dry_run` is TRUE. A violation of the (wal_id, analyte) unique
#' constraint is reported as already-loaded wal ids.

load_prs <- function(con, prs_data, dry_run = FALSE) {

  DBI::dbBegin(con)

  inserted <- tryCatch(
    DBI::dbAppendTable(
      conn  = con,
      name  = DBI::Id(schema = prs_schema, table = prs_table),
      value = as.data.frame(prs_data)
    ),
    error = function(e) {

      DBI::dbRollback(con)

      if (grepl("unique|duplicate key", conditionMessage(e), ignore.case = TRUE)) {

        loaded <- DBI::dbGetQuery(
          con,
          paste0(
            "SELECT DISTINCT wal_id FROM ", prs_schema, ".", prs_table,
            " WHERE wal_id IN (", paste(unique(prs_data$wal_id), collapse = ","), ")",
            " ORDER BY wal_id"
          )
        )$wal_id

        stop(
          length(loaded), " of ", length(unique(prs_data$wal_id)),
          " WAL IDs already exist in ", prs_schema, ".", prs_table,
          " (e.g. ", paste(utils::head(loaded, 5), collapse = ", "),
          "). Nothing was written.",
          call. = FALSE
        )

      }

      stop(conditionMessage(e), call. = FALSE)

    }
  )

  if (dry_run) {
    DBI::dbRollback(con)
  } else {
    DBI::dbCommit(con)
  }

  return(inserted)

}
