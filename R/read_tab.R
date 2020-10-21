
#' @title Read contents of a selected table
#'
#' @description The function reads contents of a selected table and
#'     prints it to the console.
#'
#' @param table a name of a table to read from,
#' @param field names of columns to print,
#' @param sel_fun a function selecting rows according to a user defined criterion.
#' @param ... argument passed to \code{print} function
#'
#' @export

read_tab <- function (table, field, sel_fun = NULL, ...) {

  test_table_arg(table)
  test_field_arg(table, field)

  if (is.null(sel_fun)) {
    print((geo_data[[table]])[, field], ...)
  } else if (!is.function(sel_fun)) {
    stop ("\"sel_fun\" is not a function.")
  } else {

    if (length(formals(sel_fun)) != 1) {
      stop ("\"sel_fun\" should take only \"table\" as an argument.")
    }

    print((geo_data[[table]])[sel_fun(table), field], ...)
  }
}
