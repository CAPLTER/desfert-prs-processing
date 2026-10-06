root <- normalizePath(file.path(testthat::test_path(), "..", ".."))

for (helper in c("helper_read_data.R", "helper_validate_deployment.R", "helper_validate_wal_ids.R", "helper_format_data.R", "helper_db.R", "helper_run.R")) {
  source(file.path(root, helper))
}

# example input files are not tracked in git (*.xlsx is ignored)
example_files <- list.files(root, pattern = "\\.xlsx$", full.names = TRUE)
