#' Run MC Test Analysis App
#'
#' Runs the MC Test ANalysis Shiny app.
#'
#' @export
explore <- function() {
  appDir <- system.file("shiny-apps", "mctestanalysis", package = "MCTestAnalysis")
  if (appDir == "") {
    stop("Could not find shiny-apps directory. Try re-installing the MCTestAnalysis package.")
  }

  suppressWarnings(shiny::runApp(appDir, display.mode = 'normal'))
}
