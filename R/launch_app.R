#' Launch the Shiny App
#'
#' This function launches the Shiny app contained in this package.
#'
#' @export
launch_app <- function() {
  app_dir <- system.file("shiny/app", package = "NRP.web")
  if (app_dir == "") {
    stop("Could not find app directory. Try re-installing `NRP.web`.", call. = FALSE)
  }
  shiny::runApp(app_dir, display.mode = "normal")
}
