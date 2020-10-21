
#' @title Load table to the "geo_data" environment
#'
#' @description The function loads a table created by a user
#'     (e.g. with a "join_tab" function) to the "geo_data" environment.
#'
#' @param table a table to upload,
#' @param table_name a name of table in "geo_data" environment.
#'
#' @export

upload_table <- function (table, table_name) {

  if (!is.data.frame(table)) {
    stop ("\"table\" is not a data frame.")
  }

  if (is.null(attr(table, "key"))) {
    stop ("There is no \"key\" attribute for the \"table\".")
  }

  if (length(unique(table[attr(table, "key")])) == ncol(table)) {
    stop ("Entries for the \"key\" column are not unique.")
  }

  if (!is.character(table_name)) {
    stop ("\"table_name\" is not a character string.")
  }

  if (table_name %in% names(geo_data)) {
    stop ("An object with the name \"table_name\" already exists in \"geo_data\" environment.")
  }

  geo_data[[table_name]] <- table
}
