#' Create MC Test Analysis Report
#'
#' Creates a static PDF file containing a report on the properties of the test
#' using Classic Test Theory and Item Response Theory.
#'
#' @inheritParams loadAllData
#' @export
createReport <- function(answer_file = file.choose(),
                         test_file = file.choose(),
                         test_title = ifelse(interactive(),
                                        readline('Test title: '), 'MC Test Analysis'),
                         author = ifelse(interactive(),
                                         readline('Test author: '), Sys.info()['user']),
                         out_file = paste0('Report', ' - ', test_title, '.pdf'),
                         out_path = getwd(),
                         open_file = TRUE,
                         report_options = list(),
                         ...) {
  cat('Using answer key: ')
  cat(answer_file, '\n')
  cat('Using test file: ')
  cat(test_file, '\n')
  title <- paste('MC Test Analysis Report:', test_title)
  author <- author

  cat("Loading data...")
  mctd <- loadAllData(answer_file, test_file, ...)
  mctd <- requires(mctd, c('item.analysis', 'irt_models'))
  cat('done.\n')

  cat("Creating report...")
  suppressWarnings(
    output_file <- rmarkdown::render(
      system.file('rmd/mct-report.Rmd', package = 'MCTestAnalysis'),
      output_format = 'pdf_document',
      output_file = out_file,
      output_dir = out_path,
      quiet = TRUE
    )
  )
  cat('done.\n')

  tryCatch({
    rstudioapi::verifyAvailable()
    rstudioapi::viewer(output_file)
    cat('Report saved to: ', output_file)
  },
  error = function(e) message('RStudio not running, report written to: ', output_file)
  )
}


createReportFromMCTD <- function(mctd,
                                 test_title = "Test Title",
                                 author = 'Author',
                                 file) {
  title <- paste('MC Test Analysis Report:', test_title)
  author <- author

  rmarkdown::render(
    system.file('rmd/mct-report.Rmd', package = 'MCTestAnalysis'),
    output_format = 'pdf_document',
    output_file = file,
    quiet = TRUE
  )
}
