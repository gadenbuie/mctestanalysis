#
# This is the server logic of a Shiny web application. This application creates
# the interface for loading your test data and running the mctestanalysis
# package on your testing data.

library(shiny)
library(MCTestAnalysis)

shinyServer(function(input, output, session) {

  # Reactive data elements ----
  mctd <- reactive({
    if (is.null(input$f_test) | is.null(input$f_answer_key)) return(NULL)
    x <- loadAllData(answer_file    = input$f_answer_key$datapath,
                     test_file      = input$f_test$datapath,
                     has_student_id = input$o_import_has_student_id,
                     na.strings     = ifelse(input$o_import_missing_id == 'Blank',
                                             'NA', input$o_import_missing_id),
                     header         = input$o_import_header,
                     sep            = input$o_import_sep,
                     quote          = input$o_import_quote)
    return(x)
  })

  concepts <- reactive({
    # Set concepts
    x <- setNames(mctd()$AnswerKey$Concept, mctd()$AnswerKey$Question)
    # Make concepts available
    return(x)
  })

  observe({
    x <- concepts()
    updateSelectizeInput(session, 'o_overallbox_concepts', choices = unique(x), selected = unique(x))
  })

  # Import Data Outputs ----

  output$t_data_check <- renderText({
    summary_text <- c()

    add_to_output <- function(...) {
      element <- paste(...)
      summary_text <<- c(summary_text, element)
    }

    is_loaded_answer_key <- !is.null(input$f_answer_key)
    is_loaded_test <- !is.null(input$f_test)
    if (!is_loaded_answer_key) {
      add_to_output('Please upload answer key.')
    }

    if (!is_loaded_test) {
      add_to_output('Please upload test results.')
    }

    if (is_loaded_answer_key & is_loaded_test) {
      add_to_output('Questions:')
      add_to_output('  - In answer key:', length(mctd()$AnswerKey$Question))
      add_to_output('  - In test data: ', ncol(mctd()$Test))
      add_to_output('')
      add_to_output('Responses:')
      add_to_output('  - Incomplete:', nrow(mctd()$Test[!complete.cases(mctd()$Test),]))
      add_to_output('  - Total:', nrow(mctd()$Test))
      add_to_output('')
      add_to_output('Concepts:', length(unique(mctd()$AnswerKey$Concept)))
    }

    paste(summary_text, collapse = '\n')
  })

  output$down_answer_key_example <- downloadHandler(
    filename = function() {'answer_key_example.csv'},
    content = function(file) {write.csv(answer_key_example, file)}
  )

  output$down_test_example <- downloadHandler(
    filename = function() {'test_example.csv'},
    content = function(file) {write.csv(test_example, file)}
  )

  # ---- View Test Results ----
  output$t_answer_key <- renderDataTable({
    if (is.null(input$f_answer_key)) return(NULL)
    mctd()$AnswerKey
  }, options = list('pageLength' = 50))

  output$t_test <- renderDataTable({
    if (is.null(input$f_test)) return(NULL)
    mctd()$Test %>%
      tibble::rownames_to_column('id') %>%
      select(id, everything())
  }, options = list('pageLength' = 10))

  output$t_option_pct <- renderDataTable({
    if(is.null(mctd())) return(NULL)
    x <- optionsSelectedPct(mctd(),
                            include_title = TRUE,
                            questions_as_row_names = FALSE,
                            correct_vs_incorrect = input$o_option_pct_correct == 'Correct v. Incorrect')
    if (input$o_option_pct_count == 'Percentage') {
      x[, -1:-2] <- round(x[, -1:-2]/nrow(mctd()$Test)*100, 2)
    }
    x <- switch(input$o_option_pct_cols,
                'Question' = x[, -2],
                'Question Title' = x[, -1],
                'Both' = x)
  }, options = list('pageLength' = 10))

  # ---- Classic Test Theory ----

  summarize_ctt <- reactive({
    if(is.null(mctd())) return(NULL)
    summarizeCTT(mctd(), input$o_classic_summary_table == 'Test Summary')
  })

  output$t_classic_summary <- DT::renderDataTable(
    DT::datatable(summarize_ctt(),
                  filter = ifelse(input$o_classic_summary_table == 'Test Summary', 'none', 'bottom'),
                  autoHideNavigation = TRUE,
                  rownames = FALSE,
                  fillContainer = FALSE,
                  options = list(
                    'pageLength' = 10,
                    # 'autoWidth' = TRUE,
                    'searching' = !(input$o_classic_summary_table == 'Test Summary'),
                    'paging' = !(input$o_classic_summary_table == 'Test Summary')
                  )
    )
  )

  output$txt_classic_summary <- renderUI({
    if (input$o_classic_summary_table == 'Test Summary') {
      helpText(
        tags$p(tags$strong("Cronbach's Alpha."),
               "the parameter cronbach alpha is a measure of internal consistency.",
               "In other words, how closely related a set of items are as a group.",
               "It is considered to be a measure of scale reliability.",
               "Technically speaking, Cronbach’s alpha is not a statistical routine or test;",
               "instead, it is a coefficient of reliability (or consistency).",
               tags$a(href="http://www.ats.ucla.edu/stat/spss/faq/alpha.html",
                      "What Does Cronbach's Alpha Mean?")
        )
      )
    } else {
      helpText(
        tags$p(tags$strong("Alpha without item (WOI)"),
               "provides a coefficient of internal reliability",
               "of the text were the item excluded."
        ),
        tags$p(tags$strong("Difficulty Index"),
               "is the proportion of students who answered the test item accurately."
        ),
        tags$p(tags$strong("Item Variance"),
               "measures the spread among item responses."
        ),
        tags$p(tags$strong("Discrimination Index"),
               "indicates the ability of the assessment to differentiate between high and low scorers."
        ),
        tags$p(
          tags$strong("Point Biserial Correlation Coefficient"),
          "(PBCC) measures correlation between correctly answering a given item",
          "and overall test scores."
        )
      )
    }
  })

  output$p_discrimination <- renderPlot({
    if (is.null(mctd())) return(NULL)
    # Parse choices
    range <- switch(paste0(input$o_disc_y_range, input$o_disc_x_range),
                    'FreeFree'         = NULL,
                    'FreePositive'     = 'max_x',
                    'PositiveFree'     = 'max_y+',
                    'PositivePositive' = 'max_all+',
                    'FullFree'         = 'max_y',
                    'FullPositive'     = 'max_all')
    discriminationDifficultyPlot(mctd(),
                                 type = input$o_disc_type,
                                 show_labels = 'Labels' %in% input$o_disc_show,
                                 hide_legend = !('Legend' %in% input$o_disc_show),
                                 show_guidelines = 'Guidelines' %in% input$o_disc_show,
                                 max_limits = range)
  })

  output$p_overallbox <- renderPlot({
    if (is.null(mctd())) return(NULL)
    testScoreByQuestionPlot(mctd(),
                            concepts = input$o_overallbox_concepts,
                            facet_by_concept = input$o_overallbox_facet)
  })
})
