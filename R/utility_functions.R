
#' @title Create a function selecting rows
#'
#' @description The function uses conditions provided by a user to create
#'     a selection function which role is to select rows printed by
#'     \code{read_tab} function.
#'
#' @param table a name of a table,
#' @param ... arguments of character strings type, describing conditions.
#'
#' @export

sel_row <- function (table, ...) {

  if (!is.character(table)) {
    stop ("\"table\" is not a character string.")
  }

  if (length(ls(envir = geo_data)) == 0) {
    stop ("The \"geo_data\" environment is empty.")
  }

  if (!(table %in% list_tables(geo_data))) {
    stop ("There is no \"table\" in \"geo_data\" environment.")
  }

  if (!all(sapply(list(...), function(x) is.character(x)))) {
    stop ("Not all conditions in \"...\" are character strings.")
  }

  args <- gsub(" ", "", c(...), fixed = TRUE)
  args <- paste0("(geo_data[[table]])$", args)

  sel_fun <- function(table) {
    sel <- TRUE

    for (i in 1:length(args)) {
      sel <- sel & eval(parse(text = args[i]))
    }

    return (sel)
  }

  return (sel_fun)
}

list_tables <- function (env) {
  is_table <- function (x) {
    is.data.frame(get(x, envir = env))
  }

  return (ls(envir = env)[sapply(ls(envir = env), is_table)])
}

test_table_arg <- function (table) {
  if (!is.character(table)) {
    stop ("\"table\" is not a character string.")
  }

  if (length(ls(envir = geo_data)) == 0) {
    stop ("The \"geo_data\" environment is empty.")
  }

  if (!(table %in% list_tables(geo_data))) {
    stop ("There is no \"table\" in \"geo_data\" environment.")
  }
}

test_field_arg <- function (table, field) {
  if (!is.character(field)) {
    stop ("\"field\" is not a character string.")
  }

  if (!all(field %in% names(geo_data[[table]]))) {
    stop ("Wrong value in \"field\".")
  }
}

test_row_key_arg <- function (table, row_key) {
  key_name <- attributes(geo_data[[table]])$key

  if (!(is.vector(row_key) & !is.list(row_key) & length(row_key) == 1L)) {
    stop ("\"row_key\" is not a vector of length one.")
  }

  if (typeof(row_key) != typeof(geo_data[[table]][[key_name]])) {
    stop ("Wrong type of a \"row_key\" argument.")
  }

  if (!(any(row_key == geo_data[[table]][key_name]))) {
    stop ("There is no row with \"row_key\" id in \"table\".")
  }
}

