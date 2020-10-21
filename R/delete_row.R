
#' @title Delete a row from a selected table
#'
#' @description The function deletes a row from a selected table.
#'
#' @param table a name of the modified table,
#' @param row_key a unique name identifing a row.
#'
#' @export

delete_row <- function (table, row_key) {

  test_table_arg(table)
  test_row_key_arg(table, row_key)

  key_name <- attributes(geo_data[[table]])$key
  row_del <- which(geo_data[[table]][key_name] == row_key)

  geo_data[[table]] <- geo_data[[table]][-row_del, ]

}
