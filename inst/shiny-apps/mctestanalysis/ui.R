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
      # Give a little space beteen tabs and tab-content
      tags$head(tags$style(HTML(
        ".tab-content { padding-top: 10px; }"
      ))),
      # ---- Import Data ----
      h3("Import Test Data"),
      tabsetPanel(
        tabPanel("Import Data",
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
                 ),
                 tags$div(
                   class = 'text-right',
                   actionButton('b_load_data', 'Load Data', class = 'btn-primary')
                 )
        ),
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
                                 checkboxGroupInput('o_option_pct_cols',
                                                    label = 'Show columns',
                                                    choices = c("Title", "Answer", "Concept"),
                                                    selected = c("Title", "Answer", "Concept"),
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
                        DT::dataTableOutput('t_option_pct'))
             )
           )
  ),
  # ---- Analysis Results Dropdown Menu ----
  navbarMenu(
    'Analysis',
    tabPanel("Classic Test Theory",
             fluidPage(
               h3("Classic Test Theory Results"),
               tabsetPanel(
                 tabPanel("Summary",
                          withMathJax(),
                          fluidRow(
                            column(8, uiOutput('txt_classic_summary')),
                            column(4,
                                   wellPanel(
                                     selectInput('o_classic_summary_table',
                                                 label = 'Summary View',
                                                 choices = c('Test Summary' = 'whole',
                                                             "Concept Subgroup Summary" = 'concept',
                                                             "Item Summary" = 'item')
                                     )
                                   )
                            )
                          ),
                          DT::dataTableOutput('t_classic_summary', width = 'auto')
                 ),
                 tabPanel("Discrimination Index",
                          helpText("Help text about this plot"),
                          fluidRow(
                            column(4,
                                   selectInput('o_disc_type',
                                               label = 'Method',
                                               choices = c("Conventional", "PBCC", "Modified PBCC" = 'pbcc_modified'))
                            ),
                            column(4,
                                   checkboxGroupInput('o_disc_show', 'Show/Hide', choices = c('Labels', 'Legend', 'Guidelines'), selected = c('Labels', 'Guidelines'), inline = TRUE)
                            ),
                            column(4,
                                   radioButtons('o_disc_y_range', label = "Y-Axis", choices = c('Free', 'Positive', 'Full'), inline = TRUE),
                                   radioButtons('o_disc_x_range', label = 'X-Axis', choices = c('Free', 'Positive'), selected = 'Positive', inline = TRUE)
                            )
                          ),
                          fluidRow(
                            column(8, offset = 2,
                              plotOutput('p_discrimination')
                            )
                          )
                 ),
                 tabPanel("Overall vs. Question Score",
                          helpText("This plot compares the overall test scores against correct selection of individual items.",
                                   "Generally, it is best for the boxplot of the correct group to be mostly above the boxplot of the incorrect group.",
                                   "Questions that have complete overlap between the two boxplots should be reviewed."),
                          fluidRow(
                            column(9,
                                   radioButtons('o_overallbox_concepts', 'Concepts', choices = 'All', inline = TRUE)
                            ),
                            column(3,
                                   tags$strong('Options'),
                                   checkboxInput('o_overallbox_facet', 'Group Items by Concept?'))
                          ),
                          fluidRow(
                            column(8, offset = 2,
                              plotOutput('p_overallbox')
                            )
                          )
                 ),
                 tabPanel("Item Review",
                          helpText("This table uses a number of heuristics and guidelines ",
                                   "to guide the user in reviewing individual items",
                                   "when deciding to keep, modify or discard test items."),
                          fluidRow(
                            column(4,
                                   checkboxGroupInput('o_item_review_cols',
                                                      label = 'Show columns',
                                                      choices = c("Title", "Concept", 'Alpha WOI', 'Difficulty', 'Discrimination', 'PBCC'),
                                                      selected = c("Title", "Concept"),
                                                      inline = TRUE)
                            ),
                            column(8,
                                   wellPanel(
                                     radioButtons('o_item_review_help_group',
                                                 'Description of Review Criteria',
                                                 choices = c('Alpha', 'Jorion', 'Versatile', 'Stringent'),
                                                 inline = TRUE),
                                     uiOutput('txt_item_review_help')
                                   )
                            )
                          ),
                          DT::dataTableOutput('t_item_review'))
               )
             )),
    tabPanel("Item Response Theory",
             fluidPage(
               h3("Item Response Theory Results"),
               tabsetPanel(
                 tabPanel("IRT Models",
                          withMathJax(),
                          uiOutput('irt_models_helptext'),
                          fluidRow(
                            column(4, selectInput('o_irt_model_summary',
                                                  'Choose Summary',
                                                  choices = c('1-PL (Rasch)' = 'PL1',
                                                              '2-PL' = 'PL2',
                                                              '3-PL' = 'PL3',
                                                              'AIC')))
                          ),
                          fluidRow(
                            column(8, offset = 2,
                                   DT::dataTableOutput('t_irt_model')
                            )
                          )
                 ),
                 tabPanel("ICC",
                          helpText("Choose to plot ICC for 1-, 2- or 3-PL IRT models. (More help text needed.)"),
                          fluidRow(
                            column(4, selectInput('o_icc_model',
                                                  'Choose Model',
                                                  choices = c('1-PL (Rasch)' = 'PL1',
                                                              '2-PL' = 'PL2',
                                                              '3-PL' = 'PL3'))),
                            column(6, checkboxGroupInput('o_icc_questions',
                                                     'Choose Questions',
                                                     choices = character(0),
                                                     inline = TRUE,
                                                     width = '100%')),
                            column(2,
                              actionButton('b_icc_questions_all', "Select All"),
                              actionButton('b_icc_questions_none', "Select None"),
                              selectInput('o_icc_questions_concept', "Concept Group", choices = NULL),
                              actionButton('b_icc_questions_concept', 'Select Concept')
                            )
                          ),
                          fluidRow(
                            column(8, offset = 2,
                              plotOutput('p_icc')
                            )
                          )
                 )
               )
             )),
    tabPanel("Factor Analysis",
             fluidPage(
               h3("Factor Analysis Results"),
               tabsetPanel(
                 tabPanel("Tetrachoric Plot",
                          helpText("Provides tetrachoric plot"),
                          fluidRow(
                            column(4,
                                   checkboxInput('o_tetra_show_concept', 'Show Concept Groups', value = TRUE)
                            )
                          ),
                          fluidRow(
                            column(8, offset = 2,
                                   plotOutput('p_tetra', height = '600px')
                            )
                          )
                 ),
                 tabPanel(
                   "Scree Plot",
                   sidebarLayout(
                     sidebarPanel(
                       uiOutput('txt_scree')
                     ),
                     mainPanel(
                       plotOutput('p_scree')
                     )
                   )
                 ),
                 tabPanel(
                   "Exploratory Factor Analysis",
                   sidebarLayout(
                     sidebarPanel(
                       tags$h4("Options"),
                       selectInput(
                         'o_efa_nfactors',
                         'Number of Factors',
                         choices = c('# of Concepts' = 0, 1:15)
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
                       ),
                       actionButton('b_run_efa', 'Run Factor Analysis', class = 'btn-primary')
                     ),
                     mainPanel(
                       tabsetPanel(
                         # tabPanel(
                         #   "Factor Loadings",
                         #   DT::dataTableOutput('t_efa_factor_loadings')
                         # ),
                         tabPanel(
                           "Factor Loadings",
                           tableOutput('t_efa_factor_loadings')
                         ),
                         tabPanel(
                           "Model Output",
                           verbatimTextOutput('t_efa_out')
                         )
                       )
                     )
                   )
                 )
               )
             )),
    # tabPanel("Diagnostic Classification Modeling")
    tabPanel("Distractor Analysis",
             fluidPage(
               h3("Distractor Analysis Results"),
               fluidRow(
                 column(6,
                   helpText(
                     "The following plot and table compare the percentage of all respondents who select a given option for each item.",
                     "These tables allow the test administrator to analize the performance of item options and to determine if the choice of distracting items reveals information about the misconceptions in students' knowledge."
                   ),
                   uiOutput('txt_distractor')
                 ),
                 column(6,
                   h4('Options'),
                   sliderInput(
                     'o_distractor_pct',
                     'Percentile for performance group',
                     min = 0, max = 0.5, step = 0.01, value = 0.33),
                   radioButtons(
                     'o_distractor_pct_relative',
                     'Show percentage relative to',
                     inline = TRUE,
                     choices = c("All respondents" = 'total', "Within group" = 'group')
                   ),
                   checkboxInput(
                     'o_distractor_show_title',
                     'Include question title in plot?'
                   )
                 )
               ),
               tabsetPanel(
                 tabPanel("Plot",
                   fluidRow(
                     column(10, offset = 1,
                            plotOutput('p_distractor', height = '500')
                     )
                   )
                 ),
                 tabPanel("Table",
                   fluidRow(
                     DT::dataTableOutput('t_distractor')
                   )
                 )
               )
             ))
  ),
  tabPanel("About")
  # Page output will go here
))


# fluidPage(
#   h3("Page Title"),
#   tabsetPanel(
#     tabPanel("Tab Title",
#              helpText("Tab Help Text"),
#              fluidRow(
#                column(4, "Option Panel 1"),
#                column(4, "Option Panel 2"),
#                column(4, "Option Panel 3")
#              ),
#              # OUTPUT
#     )
#   )
# )
