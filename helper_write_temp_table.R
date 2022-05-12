#' @title write a temporary file to a postgres schema
#'
#' @description `write_temp_table` writes a data frame or tibble to a
#' designated postgres schema at the database specified in the connection
#' information to facilitate inserting data to other tables within the schema.
#'
#' @export

write_temp_table <- function(schema_name, temporary_table_name) {

  if (DBI::dbExistsTable(pg, c(schema_name, temporary_table_name))) {

    DBI::dbRemoveTable(pg, c(schema_name, temporary_table_name))

  }

  DBI::dbWriteTable(
    conn      = pg,
    name      = c(schema_name, temporary_table_name),
    value     = get(temporary_table_name),
    row.names = FALSE
  )

}
