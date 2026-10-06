#' @title read a WesternAg PRS Excel file
#'
#' @description `read_prs_file` imports the first sheet of a WesternAg PRS
#' workbook. The header row is located by finding the `WAL #` cell in the first
#' column (rather than assuming a fixed number of header lines). Every column
#' that follows the fixed sample-description columns is treated as an analyte,
#' as the suite of analytes measured differs among campaigns. The "Method
#' Detection Limits" row that WesternAg includes is returned separately.
#'
#' @return list with `samples` (one row per sample) and `mdl` (named numeric
#' vector of method detection limits, one per analyte).
#'
#' @export

fixed_prs_columns <- c(
  "WAL #",
  "Sample ID",
  "Burial Date",
  "Retrieval Date",
  "# Anion",
  "# Cation",
  "Notes"
)

read_prs_file <- function(path) {

  if (!file.exists(path)) {
    stop("input file not found: ", path)
  }

  raw <- suppressMessages(
    readxl::read_excel(
      path         = path,
      col_names    = FALSE,
      col_types    = "text",
      .name_repair = "minimal"
    )
  )

  header_row <- which(raw[[1]] == "WAL #")

  if (length(header_row) != 1) {
    stop("could not locate a single 'WAL #' header row in ", path)
  }

  prs <- readxl::read_excel(
    path = path,
    skip = header_row - 1
  )

  # drop trailing unnamed, empty columns that Excel sometimes leaves behind
  empty_unnamed <- grepl("^\\.\\.\\.[0-9]+$", names(prs)) & vapply(prs, function(x) all(is.na(x)), logical(1))
  prs <- prs[, !empty_unnamed]

  missing_columns <- setdiff(fixed_prs_columns, names(prs))

  if (length(missing_columns) > 0) {
    stop("input is missing expected column(s): ", paste(missing_columns, collapse = ", "))
  }

  analyte_columns <- setdiff(names(prs), fixed_prs_columns)

  if (length(analyte_columns) == 0) {
    stop("no analyte columns found in input")
  }

  if (any(grepl("^\\.\\.\\.[0-9]+$", analyte_columns)) || anyDuplicated(names(prs)) > 0) {
    stop("input has unnamed or duplicated column names: ", paste(names(prs), collapse = ", "))
  }

  non_numeric <- analyte_columns[!vapply(prs[analyte_columns], is.numeric, logical(1))]

  if (length(non_numeric) > 0) {
    stop("analyte column(s) are not numeric: ", paste(non_numeric, collapse = ", "))
  }

  is_mdl <- grepl("^Method Detection Limit", prs[["WAL #"]], ignore.case = TRUE)

  if (sum(is_mdl) != 1) {
    stop("expected exactly one 'Method Detection Limits' row; found ", sum(is_mdl))
  }

  mdl <- unlist(prs[is_mdl, analyte_columns])

  if (anyNA(mdl)) {
    stop("method detection limit missing for: ", paste(names(mdl)[is.na(mdl)], collapse = ", "))
  }

  samples <- prs[!is_mdl & !is.na(prs[["Sample ID"]]), ]

  if (nrow(samples) == 0) {
    stop("no samples found in input")
  }

  return(list(samples = samples, mdl = mdl))

}
