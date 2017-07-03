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
      # ---- Tab: Import Data ----
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
  # ---- Tab: View Test Results ----
  tabPanel("View",
           fluidPage(
             h3("View Test Results"),
             tabsetPanel(
               tabPanel("Answer Key",
                        column(8, DT::dataTableOutput('t_answer_key'))
               ),
               tabPanel("Test Data",
                        column(10, DT::dataTableOutput('t_test'))
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
                        column(10, DT::dataTableOutput('t_option_pct')))
             )
           )
  ),
  # ---- Dropdown Menu: Analysis ----
  navbarMenu(
    'Analysis',
    # ---- Analysis > Tab: Classic Test Theory ----
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
                          column(8, DT::dataTableOutput('t_classic_summary', width = 'auto'))
                 ),
                 tabPanel("Histogram",
                          helpText("The plot below shows the histogram of overall test scores for all students,",
                                   "with a (scaled) normal curve overlay for reference."),
                          column(8, plotOutput('p_classic_histogram'))
                 ),
                 tabPanel("Discrimination Index",
                          helpText("The below scatter plots compares the selected measure of item discrimination with the item difficulty.",
                                   "Dotted guidelines indicate the recommended ranges for each index.",
                                   "A discrimination index (or PBCC or Modified PBCC) of less than 0.2 is not recommended,",
                                   "while item difficulty should generally be between 0.2 and 0.8."),
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
                                   "when deciding to keep, modify or discard an item."),
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
                          column(10, offset = 1, DT::dataTableOutput('t_item_review')))
               )
             )),
    # ---- Analysis > Tab: Item Response Theory ----
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
                          helpText("Plot the Item Characteristic Curves for 1-, 2- or 3-PL IRT models."),
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
    # ---- Analysis > Tab: Factor Analysis ----
    tabPanel("Factor Analysis",
             fluidPage(
               h3("Factor Analysis Results"),
               tabsetPanel(
                 tabPanel("Tetrachoric Plot",
                          column(8, helpText(
                            "The following plot shows the item-by-item", tags$em("tetrachoric correlation"),
                            "for all questions in the test. The tetrachoric correlation estimates the correlation",
                            "between two variables whose measurement is artificially dichotomized",
                            "but whose underlying joint ditribution is a bivariate normal distribution.",
                            "Structural features of the tetrachoric matrix directly correpsond to the",
                            "structure of the underlying latent variables measured by the test.",
                            "For more information and resources, visit the",
                            tags$a(href = 'http://www.personality-project.org/r/book', "Personality Project webpage.")
                          )),
                          fluidRow(
                            column(4,
                                   checkboxInput('o_tetra_show_concept', 'Show Concept Groups', value = TRUE)
                            )
                          ),
                          fluidRow(
                            column(8,plotOutput('p_tetra', height = '600px'))
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
    # ---- Analysis > Tab: Distractor Analysis ----
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
                     column(10, DT::dataTableOutput('t_distractor'))
                   )
                 )
               )
             ))
  ),
  # ---- Tab: Export ----
  tabPanel(
    "Export",
    tags$h3("Create and Download Report"),
    fluidRow(
      column(6,
        tags$h4("Test Information"),
        helpText("Download a report containing a summary of the analysis demonstrated",
                 "throughout this interface. The report will use the test data you chose",
                 'in the "Import" tab. Fill in the test details below and choose your desired',
                 "output options from the selections on the right."
        ),
        textInput('export_test_title', 'Test Title'),
        textInput('export_test_author', 'Author'),
        selectInput('export_o_out_fmt', 'Output Format', choices = c('PDF' = 'pdf', 'HTML' = 'html')),
        downloadButton('export_report', label = 'Generate Report', class = 'btn-primary'),
        helpText('Generating the report may take a little while once the button is clicked.')
      ),
      column(6,
        tags$h4("Report Settings"),
        tags$h5("IRT Settings"),
        radioButtons('export_test_pl_number', 'IRT Model',
                     choices = c('Lowest AIC' = 'Auto', 'Rasch (1 PL)' = 1, '2 PL' = 2, '3 PL' = 3),
                     selected = 1,
                     inline = TRUE),
        radioButtons("export_o_icc_group", "Plot ICC Curves",
                     choices = c('Grouped by Concept' = 'concept', 'By Item (Ungrouped)' = 'question'),
                     inline = TRUE),
        tags$hr(),
        tags$h5("Distractor Analysis Settings"),
        sliderInput(
          'export_distractor.pct',
          'Percentile for high/low performance group',
          min = 0, max = 0.5, step = 0.01, value = 0.33
        ),
        tags$hr(),
        tags$h5("Exploratory Factor Analysis"),
        helpText(
          "See the",
          tags$a(href = 'https://cran.r-project.org/web/packages/psych/',
                 tags$code("psych"), "package documentation"),
          "for more information about these options."
        ),
        selectInput(
          'export_o_efa_nfactors',
          'Number of Factors',
          choices = c('# of Concepts' = 0, 'Auto (Scree Recommendation)' = -1, 1:15)
        ),
        selectInput(
          'export_o_efa_rotate',
          'Rotation Method',
          choices = list(
            'None' = 'none',
            'Orthogonal' = c("varimax", "quartimax", "bentlerT", "equamax", "varimin", "geominT", "bifactor"),
            'Oblique' = c("Promax", "oblimin", "simplimax", "bentlerQ", "geominQ", "biquartimin", "cluster" )
          ),
          selected = 'varimax'
        ),
        selectInput(
          'export_o_efa_fm',
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
          'export_o_efa_cut',
          'Factor Loading Cutoff',
          min = 0, max = 1, value = 0.3, step = 0.05
        )
      )
    )
  ),
  # ---- Tab: About ----
  tabPanel(
    "About",
    column(
      8, offset = 2,
      tags$h3("About"),
      p(
        "Many educators design multiple-choice question examination.",
        "How do we know that these tests are valid and reliable?",
        "How can we improve upon the test by way of modifying, revising and deleting items based on student responses?"
      ),
      p(
        "In a paper in the highly regarded Journal of Engineering Education, Jorion, et al (2016) developed \"an analytical framework for evaluating the validity of concept inventory claims\".",
        "We believe that we can use this framework to help educators design their multiple-choice tests as well, especially, if they are designed as the final mastery examination in a course.",
        "An open source software to analyze a multiple-choice question examination would be encouraging to educators who have minimal programming experience and promising to contributors who would enhance the program."
      ),
      tags$hr(),
      tags$h3("Authors"),
      p(
        tags$strong("Garrick Aden-Buie"),
        tags$a(href = 'http://garrickadenbuie.com', "is a doctoral candidate"),
        "in",  tags$a(href = "http://imse.eng.usf.edu", "Industrial and Management Systems Engineering"),
        "at the University of South Florida.",
        "He is an avid R enthusiast and programmer.",
        "His research focus is on collecting, storing, processing, visualizing and learning from passive sensors networks in smart homes.",
        "He is also passionate about bringing together education, data science and interactive R tools to improve education outcomes in higher education."
      ),
      p(
        tags$strong('Autar Kaw'), "is a",
        tags$a(href = "http://www.eng.usf.edu/~kaw", "professor of mechanical engineering"),
        'and Jerome Krivanek Distinguished Teacher at the',
        tags$a(href = "http://usf.edu/", 'University of South Florida.'),
        "He is a recipient of the 2012",
        tags$a(href = "http://www.usprofessorsoftheyear.org/Winners.html", "U.S. Professor of the Year Award"),
        "from the Council for Advancement and Support of Education (CASE) and Carnegie Foundation for Advancement of Teaching.",
        "Professor Kaw's related",
        tags$a(href = "http://www.eng.usf.edu/~kaw/research/", "main scholarly interests"),
        "are in engineering education research, open courseware development, and the state and future of higher education.",
        "His education research has been funded by National Science Foundation since 2002."
      ),
      tags$hr(),
      tags$h3("References"),
      tags$h4("Test Theory References"),
      REFERENCES_THEORY,
      tags$h4("Packages Used"),
      REFERENCES_PKGS,
      p(tags$em("MCTestAnalysis"), 'was built in R using the following packages:'),
      tags$ul(
        pkg_url_li('psych'),
        pkg_url_li('psychometric'),
        pkg_url_li('ltm'),
        pkg_url_li('shiny'),
        pkg_url_li('dplyr'),
        pkg_url_li('ggplot2'),
        pkg_url_li('reshape2'),
        pkg_url_li('rmarkdown'),
        pkg_url_li('DT'),
        pkg_url_li('tibble'),
        pkg_url_li('gridExtra'),
        pkg_url_li('miniUI')
      )
    )
  )
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
