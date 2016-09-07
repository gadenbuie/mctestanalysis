#
# This is the user-interface definition of a Shiny web application. This
# application creates the interface for loading your test data and running the
# mctestanalysis package on your testing data.

library(shiny)

shinyUI(navbarPage(
  title = 'MC Test Analysis',
  tabPanel("Import", {
    fluidPage(
      theme = 'lumen.css',
      # ---- Import Data ----
      h3("Import Test Data"),
      tabsetPanel(
        tabPanel("Import Data",
                 p(),
                 fluidRow(
                   column(4,
                          wellPanel(
                            h4("Import Settings"),
                            helpText('MC Test Analysis can import data in CSV or TSV form.',
                                     'Choose the appropriate settings for your data files below',
                                     'and then upload the answer key and students\' test results',
                                     'data in the columns to the right.'),
                            tags$hr(),
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
                   ),
                   column(4,
                          wellPanel(
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
                   ),
                   column(4,
                          wellPanel(
                            h4("Test Data"),
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
                   )
                 )),
        tabPanel("Check Data", p(),
                 verbatimTextOutput('t_data_check'))
      )
    )
  }),
  # ---- View Test Results ----
  tabPanel("View",
           fluidPage(
             h3("View Test Results"),
             tabsetPanel(
               tabPanel("Answer Key",
                        dataTableOutput('t_answer_key')
               ),
               tabPanel("Test Data",
                        dataTableOutput('t_test')
               ),
               tabPanel("Option Selection",
                        helpText("This table shows the number of students who ",
                                 "chose a given option for each question."),
                        fluidRow(
                          column(4,
                                 radioButtons('o_option_pct_cols',
                                              label = 'Show columns',
                                              choices = c('Question', 'Question Title', 'Both'),
                                              selected = 'Both',
                                              inline = TRUE)
                          ),
                          column(3,
                                 radioButtons('o_option_pct_count',
                                              label = 'Display As',
                                              choices = c('Count', 'Percentage'),
                                              inline = TRUE)
                          ),
                          column(5,
                                 radioButtons('o_option_pct_correct',
                                              label = 'Option Grouping',
                                              choices = c('All Options', 'Correct v. Incorrect'),
                                              inline = TRUE)
                          )
                        ),
                        dataTableOutput('t_option_pct'))
             )
           )
  ),
  # ---- Analysis Results Dropdown Menu ----
  navbarMenu(
    'Analysis',
    tabPanel("Concept Inventory"),
    tabPanel("Item Response Theory"),
    tabPanel("Factor Analysis"),
    tabPanel("Diagnostic Classification Modeling")
  ),
  tabPanel("About")
  # Page output will go here
))
