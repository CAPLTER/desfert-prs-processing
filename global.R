#' @section helper scripts

source("helper_write_temp_table.R")
source("helper_validate_deployment.R")
source("helper_format_data.R")


#' @section analyte names and detection details for use in data processing

#' @description Unique analyte names when imported with read_excel() and after
#' renaming `Total N` to `Total-N`:

analytes <- c(
  "Total-N",
  "NO3-N",
  "NH4-N",
  "Ca",
  "Mg",
  "K",
  "P",
  "Fe",
  "Mn",
  "Cu",
  "Zn",
  "B",
  "S",
  "Pb",
  "Al",
  "Cd"
)

#' @description detection-limits

two <- c(
  "Total-N",
  "NO3-N",
  "NH4-N",
  "Ca",
  "S"
)

four <- c(
  "Mg",
  "K"
)

two_tenths <- c(
  "P",
  "Mn",
  "Cu",
  "Zn",
  "B",
  "Pb",
  "Cd"
)

four_tenths <- c(
  "Fe",
  "Al"
)
