#' Run MC Test Analysis App
#'
#' Runs the MC Test ANalysis Shiny app.
#'
#' @export
run_app <- function() {
  appDir <- system.file("shiny-apps", "mctestanalysis", package = "MCTestAnalysis")
  if (appDir == "") {
    stop("Could not find shiny-apps directory. Try re-installing the MCTestAnalysis package.")
  }

  shiny::runApp(appDir, display.mode = 'normal')
}
