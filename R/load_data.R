#' @title Load geographic database
#'
#' @description The function loads three data frames (city, country and
#'     country language) to a new environment named \code{geography_data}.
#'     It allows package functions to work on data frames without copying them.
#'
#' @export

load_data <- function () {
  # add simple check if geo_data env is already presented
  # assign("geo_data", new.env(parent = globalenv()), envir = globalenv())
  utils::data("geography", package = "geography", envir = geo_data)
}
