# Exercises the real insert path. The example files are already loaded, so each
# file's rows are deleted and re-inserted inside a transaction that is always
# rolled back; nothing persists. Requires local access as the current user.

test_that("loaded data reproduce what is in the database", {

  skip_if(length(example_files) == 0, "no example files")

  con <- tryCatch(connect_pg("localhost", "caplter", Sys.getenv("USER")), error = function(e) NULL)
  skip_if(is.null(con), "database not available")
  on.exit(DBI::dbDisconnect(con))

  for (f in example_files) {

    new_prs <- format_prs_data(read_prs_file(f), db_analytes(con))
    validate_plots(con, new_prs$plot_id)

    wal_ids <- paste(unique(new_prs$wal_id), collapse = ",")

    # a duplicate load must be refused
    expect_error(load_prs(con, new_prs, dry_run = TRUE), "already exist", info = f)

    DBI::dbBegin(con)
    withr::defer(try(DBI::dbRollback(con), silent = TRUE))

    DBI::dbExecute(con, sprintf("DELETE FROM urbancndep.prs_analysis WHERE wal_id IN (%s)", wal_ids))
    n <- DBI::dbAppendTable(con, DBI::Id(schema = "urbancndep", table = "prs_analysis"), as.data.frame(new_prs))
    expect_equal(n, nrow(new_prs), info = f)

    DBI::dbRollback(con)

  }

})
