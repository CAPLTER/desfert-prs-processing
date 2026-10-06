test_that("every example file reads and formats", {

  skip_if(length(example_files) == 0, "no example files")

  for (f in example_files) {

    prs <- read_prs_file(f)
    out <- format_prs_data(prs)

    expect_equal(nrow(out), nrow(prs$samples) * length(prs$mdl), info = f)
    expect_true(all(out$location_within_plot %in% c("under plant", "between plant", "BLANK")), info = f)
    expect_false("Total N" %in% out$analyte, info = f)
    expect_true(all(out$flag %in% c("below detection limit", NA)), info = f)

  }

})

test_that("detection-limit flag uses <= the file's MDL", {

  skip_if(length(example_files) == 0, "no example files")

  prs <- read_prs_file(example_files[1])
  out <- format_prs_data(prs)
  mdl <- prs$mdl
  names(mdl)[names(mdl) == "Total N"] <- "Total-N"

  expect_equal(
    !is.na(out$flag),
    unname(out$final_value <= mdl[out$analyte])
  )

})

modified <- function(change) {

  skip_if(length(example_files) == 0, "no example files")

  prs <- read_prs_file(example_files[1])
  change(prs)

}

test_that("validation failures stop", {

  expect_error(format_prs_data(modified(function(p) { names(p$samples)[names(p$samples) == "Ca"] <- "Ca2"; names(p$mdl)[names(p$mdl) == "Ca"] <- "Ca2"; p })), "unrecognized analyte")
  expect_error(format_prs_data(modified(function(p) { p$samples[["Sample ID"]][2] <- "2X"; p })), "Sample ID")
  expect_error(format_prs_data(modified(function(p) { p$samples[["Sample ID"]][2] <- p$samples[["Sample ID"]][1]; p })), "duplicated Sample ID")
  expect_error(format_prs_data(modified(function(p) { p$samples[["WAL #"]][2] <- p$samples[["WAL #"]][1]; p })), "duplicated WAL")
  expect_error(format_prs_data(modified(function(p) { p$samples[["Retrieval Date"]][1] <- "2099-01-01"; p })), "deployment")
  expect_error(format_prs_data(modified(function(p) { p$samples[["Burial Date"]][1] <- "not a date"; p })), "unparseable")

})

test_that("a subset of analytes is accepted", {

  out <- format_prs_data(modified(function(p) {
    keep <- c("Total N", "NO3-N", "NH4-N")
    p$samples <- p$samples[c(fixed_prs_columns, keep)]
    p$mdl <- p$mdl[keep]
    p
  }))

  expect_setequal(out$analyte, c("Total-N", "NO3-N", "NH4-N"))

})

test_that("read_prs_file rejects bad input", {

  expect_error(read_prs_file("does-not-exist.xlsx"), "not found")

})
