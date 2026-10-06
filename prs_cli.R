# Command-line entry point; typically invoked via prs_load.sh.
#
#   Rscript prs_cli.R --input FILE --user U [--host H] [--dbname D] [--port P] [--dry-run]
#
# Exits non-zero (with the error on stderr) on any failure.

script_dir <- dirname(sub("^--file=", "", grep("^--file=", commandArgs(), value = TRUE)[1]))

for (helper in c("helper_read_data.R", "helper_validate_deployment.R", "helper_validate_wal_ids.R", "helper_format_data.R", "helper_db.R", "helper_run.R")) {
  source(file.path(script_dir, helper))
}

parse_args <- function(args) {

  opts <- list(
    input   = NULL,
    host    = "localhost",
    dbname  = "caplter",
    user    = NULL,
    port    = 5432,
    dry_run = FALSE
  )

  i <- 1

  while (i <= length(args)) {

    flag <- args[i]

    if (flag == "--dry-run") {
      opts$dry_run <- TRUE
      i <- i + 1
      next
    }

    if (!flag %in% c("--input", "--host", "--dbname", "--user", "--port") || i == length(args)) {
      stop("invalid or incomplete argument: ", flag)
    }

    opts[[sub("^--", "", flag)]] <- args[i + 1]
    i <- i + 2

  }

  if (is.null(opts$input)) {
    stop("--input is required")
  }

  if (is.null(opts$user)) {
    stop("--user is required")
  }

  opts$port <- suppressWarnings(as.integer(opts$port))

  if (is.na(opts$port)) {
    stop("--port must be an integer")
  }

  opts

}

status <- tryCatch(
  {
    opts <- parse_args(commandArgs(trailingOnly = TRUE))
    do.call(run_prs_load, opts)
    0
  },
  error = function(e) {
    message("ERROR: ", conditionMessage(e))
    1
  }
)

quit(save = "no", status = status)
