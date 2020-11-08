
#' @title Add a row to a selected table
#'
#' @description The function adds a row to a selected table.
#'     Cells which are not provided by a user are replaced with \code{NA}.
#'
#' @param table a name of the modified table,
#' @param field names of columns,
#' @param value inserted values.
#'
#' @export

add_row <- function (table, field, value) {

  test_table_arg(table)
  test_field_arg(table, field)

  if (length(field) != length(value)) {
    stop ("\"field\" and \"value\" vectors are of different length.")
  }

  key_name <- attributes(geo_data[[table]])$key

  if (!(key_name %in% field)) {
    stop ("There is no \"key\" name in the \"field\" vector.")
  }

  key_type <- typeof(geo_data[[table]][[key_name]])

  res <- tryCatch(
    {
    storage.mode(value[which(field == key_name)]) <- key_type
    0L
    },
    warning = function (cond) {1L},
    error = function (cond) {2L}
  )

  if (res != 0L) {
    stop ("The \"key\" value has a wrong type.")
  }

  if (any(geo_data[[table]][key_name] == value[field == key_name])) {
    stop ("The \"key\" value is not unique.")
  }

  types <- sapply(geo_data[[table]], typeof)

  new_row <- data.frame(t(rep(NA, ncol(geo_data[[table]]))))
  names(new_row) <- names(geo_data[[table]])

  for (i in 1:length(field)) {
    new_row[field[i]] <- value[i]
  }

  for (i in 1:length(new_row)) {
    storage.mode(new_row[i]) <- types[i]
  }

  geo_data[[table]][nrow(geo_data[[table]])+1, ] <- new_row

}
