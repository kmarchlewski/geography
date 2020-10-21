
#' @title Join selected tables
#'
#' @description The function joins (SQL: inner join) selected tables
#'     with respect to a key.
#'
#' @param table_1 a name of the first table,
#' @param table_2 a name of the second table,
#' @param key_col_1 a column with unique names in the first table,
#' @param key_col_2 a column with unique names in the second table,
#' @param sel_fun a function selecting rows according to a user defined criterion.
#'
#' @return a resulting data frame
#'
#' @export

join_tab <- function (table_1, table_2, key_col_1, key_col_2, sel_fun = NULL) {

  test_table_arg(table_1)
  test_table_arg(table_2)

  if (!(
    is.vector(key_col_1) & !is.list(key_col_1) &
    is.character(key_col_1) & length(key_col_1) == 1L
  )) {
    stop ("\"key_col_1\" is not a character string of length one.")
  }

  if (!(
    is.vector(key_col_2) & !is.list(key_col_2) &
    is.character(key_col_2) & length(key_col_2) == 1L
  )) {
    stop ("\"key_col_2\" is not a character string of length one.")
  }

  if (!(key_col_1 %in% names(geo_data[[table_1]]))) {
    stop ("There is no \"key_col_1\" in the \"table_1\".")
  }

  if (!(key_col_2 %in% names(geo_data[[table_2]]))) {
    stop ("There is no \"key_col_2\" in the \"table_2\".")
  }

  if (!is.null(sel_fun)) {
    if (!is.function(sel_fun)) {
      stop ("\"sel_fun\" is not a function.")
    }

    if (length(formals(sel_fun)) != 1) {
      stop ("\"sel_fun\" should take only \"table\" as an argument.")
    }
  }

  tab_join <- merge(
    geo_data[[table_1]], geo_data[[table_2]],
    by.x = key_col_1, by.y = key_col_2,
    suffixes = c(paste0("_", table_1), c(paste0("_", table_2)))
  )

  tab_join <- cbind(ID_join = 1:nrow(tab_join), tab_join)

  attr(tab_join, "key") <- "ID_join"

  return (tab_join)
}
