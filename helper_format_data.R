#' @title format imported prs data for upload to database
#'
#' @description `format_prs_data` converts the output of `read_prs_file` to the
#' long (one row per sample x analyte) structure of `urbancndep.prs_analysis`,
#' with columns named as in the database table. Processing includes data
#' validation (sample ids, analytes, dates, detection limits, wal ids). Stops on
#' any validation failure.
#'
#' @param prs list returned by `read_prs_file`.
#'
#' @export

# analytes that have been reported by WesternAg to date; the database is also
# consulted at load time for analytes already present.
known_analytes <- c(
  "Total-N", "NO3-N", "NH4-N", "Ca", "Mg", "K", "P", "Fe",
  "Mn", "Cu", "Zn", "B", "S", "Pb", "Al", "Cd"
)

# plot ids greater than this are lab/field blanks
max_non_blank_plot <- 75

format_prs_data <- function(prs, additional_known_analytes = character()) {

  samples <- prs$samples
  mdl     <- prs$mdl

  # WesternAg names total nitrogen `Total N`; the database uses `Total-N`
  names(samples)[names(samples) == "Total N"] <- "Total-N"
  names(mdl)[names(mdl) == "Total N"]         <- "Total-N"

  analyte_columns <- names(mdl)

  unknown <- setdiff(analyte_columns, c(known_analytes, additional_known_analytes))

  if (length(unknown) > 0) {
    stop("unrecognized analyte(s): ", paste(unknown, collapse = ", "))
  }

  # sample ids

  sample_id <- toupper(trimws(samples[["Sample ID"]]))

  if (!all(grepl("^[0-9]+[AB]$", sample_id))) {
    stop(
      "Sample ID(s) not in expected form (plot number followed by A or B): ",
      paste(samples[["Sample ID"]][!grepl("^[0-9]+[AB]$", sample_id)], collapse = ", ")
    )
  }

  if (anyDuplicated(sample_id) > 0) {
    stop("duplicated Sample ID(s): ", paste(unique(sample_id[duplicated(sample_id)]), collapse = ", "))
  }

  wal_id <- suppressWarnings(as.integer(samples[["WAL #"]]))

  if (anyNA(wal_id)) {
    stop("WAL # is missing or not an integer for sample(s): ", paste(sample_id[is.na(wal_id)], collapse = ", "))
  }

  if (anyDuplicated(wal_id) > 0) {
    stop("duplicated WAL #(s): ", paste(unique(wal_id[duplicated(wal_id)]), collapse = ", "))
  }

  # dates

  parse_date <- function(x, label) {
    parsed <- suppressWarnings(lubridate::ymd(x))
    if (any(is.na(parsed) & !is.na(x))) {
      stop("unparseable ", label, " (expected yyyy-mm-dd) for sample(s): ", paste(sample_id[is.na(parsed) & !is.na(x)], collapse = ", "))
    }
    parsed
  }

  start_date <- parse_date(samples[["Burial Date"]], "Burial Date")
  end_date   <- parse_date(samples[["Retrieval Date"]], "Retrieval Date")

  validate_deployment_times(start_date, end_date, sample_id)

  # sample-level table

  plot_id <- as.integer(sub("[AB]$", "", sample_id))

  sample_info <- data.frame(
    wal_id               = wal_id,
    plot_id              = plot_id,
    start_date           = start_date,
    end_date             = end_date,
    location_within_plot = dplyr::case_when(
      plot_id > max_non_blank_plot ~ "BLANK",
      grepl("A$", sample_id)       ~ "under plant",
      grepl("B$", sample_id)       ~ "between plant"
    ),
    num_cation_probes    = as.integer(samples[["# Cation"]]),
    num_anion_probes     = as.integer(samples[["# Anion"]]),
    notes                = samples[["Notes"]],
    stringsAsFactors     = FALSE
  )

  # long format with detection-limit flag from the detection limits reported in
  # the file

  results <- tidyr::pivot_longer(
    data      = cbind(wal_id = wal_id, samples[analyte_columns]),
    cols      = dplyr::all_of(analyte_columns),
    names_to  = "analyte",
    values_to = "final_value"
  )

  formatted_prs <- merge(sample_info, results, by = "wal_id", sort = FALSE)

  formatted_prs$flag <- ifelse(
    !is.na(formatted_prs$final_value) & formatted_prs$final_value <= mdl[formatted_prs$analyte],
    "below detection limit",
    NA_character_
  )

  formatted_prs <- formatted_prs[, c(
    "wal_id", "plot_id", "start_date", "end_date", "analyte", "final_value",
    "flag", "location_within_plot", "num_cation_probes", "num_anion_probes",
    "notes"
  )]

  validate_wal_ids(wal_id, formatted_prs$wal_id)

  if (nrow(formatted_prs) != length(wal_id) * length(analyte_columns)) {
    stop("unexpected number of rows after formatting")
  }

  return(tibble::as_tibble(formatted_prs))

}
