#' Create MC Test Analysis Report
#'
#' Creates a static PDF file containing a report on the properties of the test
#' using Classic Test Theory and Item Response Theory.
#'
#' @inheritParams loadAllData
#' @param test_title Title of the test being analyzed
#' @param author Name of the test administrator(s)
#' @param out_file Desired name of report file when saved
#' @param out_path Directory where report file should be saved, defaults to
#'   current working directory
#' @param out_fmt Either \code{'pdf'} or \code{'html'}, indicating format of
#'   report file. PDF files will be smaller and better typset, HTML files may be
#'   easier to navigate. Note that \code{pdflatex} is required to be installed.
#'   If \code{pdflatex} is not found, the report will fall back to HTML output.
#' @param open_file Should the report file be opened automatically once
#'   generated?
#' @param report_options A list of options that affect analysis parameters and
#'   results displayed inside the report. Report options include
#'   \code{irt_model_choice}, \code{icc_group} (\code{"question"} or
#'   \code{"concept"}), \code{efa.nfactors}, \code{efa.cut}, \code{efa.rotate},
#'   \code{efa.fm} and \code{distractor.pct}.
#' @export
createReport <- function(answer_file = file.choose(),
                         test_file = file.choose(),
                         test_title = ifelse(interactive(),
                                        readline('Test title: '), 'MC Test Analysis'),
                         author = ifelse(interactive(),
                                         readline('Test author: '), Sys.info()['user']),
                         out_file = paste0('Report', ' - ', test_title),
                         out_path = getwd(),
                         out_fmt  = 'pdf',
                         open_file = TRUE,
                         verbose = TRUE,
                         report_options = list(),
                         ...) {
  stopifnot(out_fmt %in% c('pdf', 'html'))
  if (verbose) cat('Using answer key: ')
  if (verbose) cat(answer_file, '\n')
  if (verbose) cat('Using test file: ')
  if (verbose) cat(test_file, '\n')
  title <- paste('MC Test Analysis Report:', test_title)
  author <- author

  if (verbose) cat("Loading data...")
  mctd <- loadAllData(answer_file, test_file, ...)
  mctd <- requires(mctd, c('item.analysis', 'irt_models'))
  if (verbose) cat('done.\n')

  if (verbose) cat("Creating report...")
  out_fmt <- choose_output_format(out_fmt)
  out_file <- paste0(out_file, '.', out_fmt)

  # Write report, but suppress output
  suppressWarnings(
    output_file <- rmarkdown::render(
      system.file('rmd/mct-report.Rmd', package = 'MCTestAnalysis'),
      output_format = output_format_function(out_fmt),
      output_file = out_file,
      output_dir = out_path,
      quiet = TRUE
    )
  )
  if (verbose) cat('done.\n')

  # Try to open the file
  if (open_file) {
    tryCatch({
      rstudioapi::verifyAvailable()
      if (out_fmt == 'html') {
        temp_file <- tempfile(fileext = '.html')
        file.copy(output_file, temp_file, overwrite = TRUE)
      } else temp_file <- output_file
      rstudioapi::viewer(temp_file)
      if (verbose) cat('Report saved to: ', output_file)
    },
    error = function(e) message('RStudio not running, report written to: ', output_file)
    )
  } else {
    if (verbose) cat('Report saved to: ', output_file)
  }
}


createReportFromMCTD <- function(mctd,
                                 test_title = "Test Title",
                                 author = 'Author',
                                 file,
                                 out_fmt = 'pdf',
                                 report_options = list()) {
  title <- paste('MC Test Analysis Report:', test_title)
  author <- author

  rmarkdown::render(
    system.file('rmd/mct-report.Rmd', package = 'MCTestAnalysis'),
    output_format = output_format_function(choose_output_format(out_fmt)),
    output_file = file,
    quiet = TRUE
  )
}

choose_output_format <- function(out_fmt) {
  if (out_fmt == 'pdf') {
    # Check that pdflatex is installed, default to html otherwise
    if (.Platform$OS.type == 'windows') {
      pdflatex_found <- system2('pdflatex.exe', '--version', stdout = NULL) == 0
    } else {
      pdflatex_found <- system('pdflatex -v', ignore.stdout = TRUE, ignore.stderr = TRUE) == 0
    }
    if (!pdflatex_found) {
      out_fmt <- 'html'
      warning('pdflatex not found, defaulting to HTML output.',
              '\nInstall MiKTeX (Windows), MacTeX (Mac), or texlive (Linux) for PDF support.')
    }
  }
  return(out_fmt)
}

output_format_function <- function(out_fmt) {
  if (out_fmt == 'pdf') {
    fmt_func <- rmarkdown::pdf_document(toc = TRUE)
  } else {
    fmt_func <- rmarkdown::html_document(
      toc = TRUE,
      toc_float = TRUE,
      theme = 'lumen'
    )
  }
  return(fmt_func)
}
