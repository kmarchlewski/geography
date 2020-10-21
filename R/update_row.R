
#' @title Update a row in a selected table
#'
#' @description The function updates a row in a selected table.
#'
#' @param table a name of the modified table,
#' @param row_key a unique name identifing a row,
#' @param field names of modified columns,
#' @param value inserted values.
#'
#' @export

update_row <- function (table, row_key, field, value) {

  test_table_arg(table)
  test_row_key_arg(table, row_key)
  test_field_arg(table, field)

  if (length(field) != length(value)) {
    stop ("\"field\" and \"value\" vectors are of different length.")
  }

  key_name <- attributes(geo_data[[table]])$key

  if (key_name %in% field) {
    stop ("You can not update a table key.")
  }

  row_sel <- which(geo_data[[table]][key_name] == row_key)
  types <- sapply(geo_data[[table]], typeof)

  for (i in 1:length(field)) {
    storage.mode(value[i]) <- types[field[i]]
    geo_data[[table]][row_sel, field[i]] <- value[i]
  }

}
