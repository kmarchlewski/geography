
#' @title A function launching the application
#'
#' @description The function launches the shiny application
#'
#' @export

run_app <- function() {
  app_dir <- system.file("shiny_app", package = "geography")

  if (app_dir == "") {
    stop("There is no the application directory.")
  }

  shiny::runApp(app_dir, display.mode = "normal")
}
