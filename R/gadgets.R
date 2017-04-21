#' Load Data via Shiny Gadget
#'
#' Use a Shiny Gadget to load the test data. Upon clicking "Load Data" a list
#' is returned containing the data required for the MCTestAnaylsis package.
#'
#' @import shiny
#' @import miniUI
#' @export
loadDataGadget <- function() {
  requireNamespace('shiny', quietly = TRUE)
  requireNamespace('miniUI', quietly= TRUE)
  ui <- miniPage(
    gadgetTitleBar("Load Test Data",
                   right = miniTitleBarButton('done', 'Load Data', primary = TRUE)
    ),
    miniTabstripPanel(
      miniTabPanel(
        "Import Settings",
        miniContentPanel(
          tags$h4('Load MC test data for analysis'),
          tags$p('Use this dialogue window to select your answer key and test results data.',
                 'When you click the "Done" button, the test data will be loaded and the',
                 'data object will be returned. This should be used to load data for manual analysis via',
                 'the functions in the MCTestAnalysis package.'),
          tags$hr(),
          import_settings_ui
        )
      ),
      miniTabPanel(
        "Load Data",
        miniContentPanel(
          fillCol(
            fillRow(
              fillCol(miniContentPanel(answer_key_ui)),
              fillCol(miniContentPanel(test_data_ui))
            )
          )
        )
      ),
      miniTabPanel(
        "Processing",
        miniContentPanel(
          tags$h4("Processing Options"),
          tags$p(
            'Choose which analysis items should be calculated and stored in the data object that will be returned.',
            "If any of these are items are needed later, they will be calculated by the package's functions",
            "when they are needed."
          ),
          checkboxGroupInput('opts_require', 'Include the following',
                             choices = c('Item Score' = 'item.score',
                                         'Item Analysis' = 'item.analysis',
                                         'Cronbach Alpha' = 'alpha',
                                         "Discrimination Index" = 'discrimination_index',
                                         "Point-Biserial Correlation" = 'pbcc',
                                         "PBCC (Modified)" = 'pbcc_modified',
                                         "IRT Models" = 'irt_models'),
                             selected = c('item.score', 'item.analysis', 'alpha',
                                          'discrimination_index', 'pbcc', 'pbcc_modified')
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Example data
    ak_file <- system.file("extdata", 'answer_key_example.csv', package = 'MCTestAnalysis')
    answer_key_example <- read.csv(ak_file, stringsAsFactors = FALSE)
    test_file <- system.file("extdata", "test_example.csv", package = "MCTestAnalysis")
    test_example <- read.csv(test_file, stringsAsFactors = FALSE)

    output$down_answer_key_example <- downloadHandler(
      filename = function() {'answer_key_example.csv'},
      content = function(file) {write.csv(answer_key_example, file, row.names = FALSE)}
    )

    output$down_test_example <- downloadHandler(
      filename = function() {'test_example.csv'},
      content = function(file) {write.csv(test_example, file, row.names = FALSE)}
    )

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      x <- load_data_shiny(input)
      if (is.null(x)) {
        showModal(modalDialog(title = 'Missing inputs',
                              paste('Answer Key or Test Results were missing.',
                                    'Please choose a file for either, or use the "Cancel" button to exit.'))
        )
      } else {
        stopApp(x)
      }
    })
  }

  runGadget(ui, server, viewer = dialogViewer('Load MC Test Data', width = 800))
}

#' Create a report using a dialogue window
#'
#' Uses a Shiny Gadget to load the test data, and generate the MC Test Analysis
#' report. The report is saved to the current working directory and opened
#' automatically in the system viewer, where the user can save it to a new
#' directory if desired.
#'
#' @import shiny
#' @import miniUI
#' @name report
createReportGadget <- function() {
  requireNamespace('shiny', quietly = TRUE)
  requireNamespace('miniUI', quietly= TRUE)
  gadget_title <- 'Create MC Test Analysis Report'
  ui <- miniPage(
    gadgetTitleBar(gadget_title),
    miniTabstripPanel(
      miniTabPanel(
        "Import Settings",
        miniContentPanel(
          tags$h4('Create a report analyzing and summarizing a multiple choice test'),
          tags$p('Use this dialogue window to select your answer key and test results data.',
                 'When you click the "Done" button, a PDF report will be generated and',
                 'opened for you.'),
          tags$hr(),
          tags$h5('Import Settings'),
          import_settings_ui
        )
      ),
      miniTabPanel(
        "Load Data",
        miniContentPanel(
          fillCol(
            fillRow(
              fillCol(miniContentPanel(answer_key_ui)),
              fillCol(miniContentPanel(test_data_ui))
            )
          )
        )
      ),
      miniTabPanel(
        "Report Settings",
        miniContentPanel(
          tags$p('Enter test details below, and then click the "Done" button to generate the report.'),
          tags$hr(),
          fillCol(
            fillRow(
              fillCol(
                tagList(
                  tags$h4("Test Information"),
                  textInput('test_title', 'Test Title'),
                  textInput('test_author', 'Author'),
                  selectInput('o_out_fmt', 'Output Format', choices = c('PDF' = 'pdf', 'HTML' = 'html'))
                )
              ),
              fillCol(
                tagList(
                  tags$h4("IRT Settings"),
                  radioButtons('test_pl_number', 'IRT Model',
                               choices = c('Lowest AIC' = 'Auto', 'Rasch (1 PL)' = 1, '2 PL' = 2, '3 PL' = 3),
                               selected = 1,
                               inline = TRUE),
                  radioButtons("o_icc_group", "Plot ICC Curves",
                               choices = c('Grouped by Concept' = 'concept', 'By Item (Ungrouped)' = 'question'),
                               inline = TRUE),
                  tags$hr(),
                  tags$h4("Distractor Analysis Settings"),
                  sliderInput(
                    'distractor.pct',
                    'Percentile for high/low performance group',
                    min = 0, max = 0.5, step = 0.01, value = 0.33
                  ),
                  tags$hr(),
                  tags$h4("Exploratory Factor Analysis"),
                  helpText(
                    "See the",
                    tags$a(href = 'https://cran.r-project.org/web/packages/psych/',
                           tags$code("psych"), "package documentation"),
                    "for more information about these options."
                  ),
                  selectInput(
                    'o_efa_nfactors',
                    'Number of Factors',
                    choices = c('# of Concepts' = 0, 'Auto (Scree Recommendation)' = -1, 1:15)
                  ),
                  selectInput(
                    'o_efa_rotate',
                    'Rotation Method',
                    choices = list(
                      'None' = 'none',
                      'Orthogonal' = c("varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor"),
                      'Oblique' = c("Promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster" )
                    ),
                    selected = 'varimax'
                  ),
                  selectInput(
                    'o_efa_fm',
                    'Factor Method',
                    choices = c('Minimum Residual' = 'minres',
                                'Weighted Least Squares' = 'wls',
                                'Generalized WLS' = 'gls',
                                'Principal Factor' = 'pa',
                                'Maximimum Likelihood' = 'ml',
                                'Minimixed Weighted Chi Square' = 'minchi',
                                'Minimum Rank Factor Analysis' = 'minrank')
                  ),
                  sliderInput(
                    'o_efa_cut',
                    'Factor Loading Cutoff',
                    min = 0, max = 1, value = 0.3, step = 0.05
                  )
                )
              )
            )
          )
        )
      )
    )
  )

  server <- function(input, output, session) {
    # Example data
    ak_file <- system.file("extdata", 'answer_key_example.csv', package = 'MCTestAnalysis')
    answer_key_example <- read.csv(ak_file, stringsAsFactors = FALSE)
    test_file <- system.file("extdata", "test_example.csv", package = "MCTestAnalysis")
    test_example <- read.csv(test_file, stringsAsFactors = FALSE)

    output$down_answer_key_example <- downloadHandler(
      filename = function() {'answer_key_example.csv'},
      content = function(file) {write.csv(answer_key_example, file, row.names = FALSE)}
    )

    output$down_test_example <- downloadHandler(
      filename = function() {'test_example.csv'},
      content = function(file) {write.csv(test_example, file, row.names = FALSE)}
    )

    # Handle the Done button being pressed.
    observeEvent(input$done, {
      showModal(modalDialog(title = 'Generating Report',
                            tags$p(paste('Processing test data and writing report, please wait.',
                                         'Depending on the number of responses and the size of the test,',
                                         'this should only take from a few seconds to a minute.')),
                            tags$p('The report will open when the process finishes.'),
                            footer = NULL))
      stopApp(
        if (!(is.null(input$f_test) | is.null(input$f_answer_key))) {
          createReport(answer_file = input$f_answer_key$datapath,
                       test_file = input$f_test$datapath,
                       test_title = input$test_title,
                       author = input$test_author,
                       out_fmt = input$o_out_fmt,
                       has_student_id = input$o_import_has_student_id,
                       na.strings     = ifelse(input$o_import_missing_id == 'Blank',
                                               'NA', input$o_import_missing_id),
                       header         = input$o_import_header,
                       sep            = input$o_import_sep,
                       quote          = input$o_import_quote,
                       report_options = list(
                         'irt_model_choice' = if (input$test_pl_number != 'Auto') input$test_pl_number,
                         'icc_group'        = input$o_icc_group,
                         'distractor.pct'   = input$distractor.pct,
                         'efa.nfactors'     = input$o_efa_nfactors,
                         'efa.rotate'       = input$o_efa_rotate,
                         'efa.fm'           = input$o_efa_fm,
                         'efa.cut'          = input$o_efa_cut
                       )
          )
        }
      )
    })
  }

  runGadget(ui, server, viewer = dialogViewer(gadget_title, width = 800))
}


#' @rdname report
#' @export
report <- function() {
  createReportGadget()
}


# ---- UI Elements ----
test_data_ui <- tagList(
  h4("Test Results"),
  fileInput('f_test', 'Choose Test Data .csv File',
            accept=c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )),
  radioButtons('o_import_missing_id',
               label = 'Missing value coded as',
               choices = c('Blank', '0', '-1', '999', '-999'),
               inline = TRUE),
  checkboxInput('o_import_has_student_id',
                'Student ID in first column',
                TRUE),
  tags$hr(),
  helpText('The test results data should contain as rows each',
           'student\'s response, with each question assigned a column.'),
  helpText('If the test results data contains student identifiers,',
           'these identifiers should be included in the first column,',
           'prior to the test answers.'),
  helpText('The MC Test Analysis Tool assumes that the question columns',
           'are in the same order as reported in the answers data.'),
  downloadLink('down_test_example', 'Download an example test data file.')
)

answer_key_ui <- tagList(
  h4("Answer Key"),
  fileInput('f_answer_key', 'Choose Answer Key .csv File',
            accept=c(
              'text/csv',
              'text/comma-separated-values',
              'text/tab-separated-values',
              'text/plain',
              '.csv',
              '.tsv'
            )),
  tags$hr(),
  helpText("The Answer Key file should contain four columns",
           "in the following order:",
           tags$ol(
             tags$li('Question number, name or identifier',
                     tags$ul(
                       tags$li(tags$em('Eg.'),
                               tags$code('Q1'), ',',
                               tags$code('1'), ', etc.')
                     )),
             tags$li('The correct answer for the question',
                     tags$ul(
                       tags$li(tags$em('Eg.'),
                               tags$code('1'), ',',
                               tags$code('A'), ', etc.')
                     )),
             tags$li('A descriptive title for the question',
                     tags$ul(
                       tags$li(tags$em('Eg.'),
                               tags$code('Talyor Function'), ',',
                               tags$code('Tensor Flow'), ', etc.')
                     )),
             tags$li('An identifier for the concept group to which',
                     'the question belongs',
                     tags$ul(
                       tags$li(tags$em('Eg.'),
                               tags$code('Taylor Series'), ',',
                               tags$code('A'), ',',
                               tags$code('Concept 1'), ', etc.')
                     ))
           ),
           downloadLink('down_answer_key_example', 'Download an example answer_key data file.')
  )
)

import_settings_ui <- tagList(
  fillRow(
    fillCol(
      helpText('MC Test Analysis can import data in CSV or TSV form.',
               'Choose the appropriate settings for your data files from the options on the right',
               'and then upload the answer key and students\' test results',
               'data in the "Load Data" tab.')
    ),
    fillCol(
      miniContentPanel(
        checkboxInput('o_import_header', 'Has Header Row', TRUE),
        radioButtons('o_import_sep', 'Separator',
                     c(Comma=',',
                       Semicolon=';',
                       Tab='\t'),
                     ','),
        radioButtons('o_import_quote', 'Quote',
                     c(None='',
                       'Double Quote'='"',
                       'Single Quote'="'"),
                     '"')
      )
    )
  )
)

# ---- Gadget Data Loading Function ----
load_data_shiny <- function(input) {
  if (is.null(input$f_test) | is.null(input$f_answer_key)) {
    return(NULL)
  } else {
    showModal(modalDialog(title = 'Loading Data',
                          paste('Processing test data, please wait.',
                                'Depending on the number of responses and the size of the test,',
                                'this should only take from a few seconds to a minute.'),
                          footer = NULL))
    x <- loadAllData(answer_file    = input$f_answer_key$datapath,
                     test_file      = input$f_test$datapath,
                     has_student_id = input$o_import_has_student_id,
                     na.strings     = ifelse(input$o_import_missing_id == 'Blank',
                                             'NA', input$o_import_missing_id),
                     header         = input$o_import_header,
                     sep            = input$o_import_sep,
                     quote          = input$o_import_quote)
    if ('opts_require' %in% names(input)) {
      x <- requires(x, input$opts_require)
    }
    removeModal()
    summary_text <- c()

    add_to_output <- function(...) {
      element <- paste(...)
      summary_text <<- c(summary_text, element)
    }

    add_to_output('Test Summary')
    add_to_output('============\n')
    add_to_output('Questions:')
    add_to_output('  - In answer key:', length(x$AnswerKey$Question))
    add_to_output('  - In test data: ', ncol(x$Test))
    add_to_output('')
    add_to_output('Responses:')
    add_to_output('  - Incomplete:', nrow(x$Test[!complete.cases(x$Test),]))
    add_to_output('  - Total:', nrow(x$Test))
    add_to_output('')
    add_to_output('Concepts:', length(unique(x$AnswerKey$Concept)))

    cat(paste(summary_text, collapse = '\n'))
    return(x)
  }
}
