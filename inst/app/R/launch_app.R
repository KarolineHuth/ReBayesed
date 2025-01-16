#' Launch the Shiny App
#'
#' This function launches the Shiny app contained in this package.
#'
#' @export
launch_app <- function() {
  app_dir <- system.file("inst", package = "ReBayesed")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `ReBayesed`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
