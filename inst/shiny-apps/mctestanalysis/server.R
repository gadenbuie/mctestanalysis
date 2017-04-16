#
# This is the server logic of a Shiny web application. This application creates
# the interface for loading your test data and running the mctestanalysis
# package on your testing data.

library(shiny)
library(MCTestAnalysis)
options(digits = 4)

shinyServer(function(input, output, session) {
  if (interactive()) {
    cat('Running in interactive mode, so will stop session at end.')
    session$onSessionEnded(stopApp)
  }

  # Reactive data elements ----
  mctd <- eventReactive(input$b_load_data, {
    if (is.null(input$f_test) | is.null(input$f_answer_key)) return(NULL)
    showModal(modalDialog(title = 'Loading Data',
                          paste('Processing test data, please wait.',
                                'Depending on the number of responses and the size of the test,',
                                'this should only take a few seconds.'),
                          footer = NULL))
    x <- NULL
    messages <- c()
    data_loaded <- FALSE
    tryCatch({
      x <- loadAllData(answer_file    = input$f_answer_key$datapath,
                       test_file      = input$f_test$datapath,
                       has_student_id = input$o_import_has_student_id,
                       na.strings     = ifelse(input$o_import_missing_id == 'Blank',
                                               'NA', input$o_import_missing_id),
                       header         = input$o_import_header,
                       sep            = input$o_import_sep,
                       quote          = input$o_import_quote)
      data_loaded <- TRUE
    }, 'error' = function(e) {messages <<- c(messages, paste(e))})
    if (!is.null(x)) {
      withCallingHandlers({
        x <- addIRTfits(x)
      },
      'warning' = function(w) {messages <<- c(messages, paste(w))},
      'error' = function(e) {messages <<- c(messages, paste(e))})
    }
    removeModal()
    if (length(messages)) {
      messages <- sub("simpleWarning[[:alnum:][:blank:]]*(in|:) ","", messages)
      messages <- unique(messages)
      messages <- strwrap(messages, 60, exdent = 4)
      if (data_loaded) {
        error_modal_body <- tagList(
          tags$p('There were warnings and/or errors during the IRT model fitting.',
                 'The messages below may provide some assistance in diagnosing the problem.',
                 'For more information, please refer to the', tags$code('ltm'), 'package documentation.')
        )
      } else {
        error_modal_body <- tags$p("Unable to load test and answer key data. Please check your data format by comparing with the example CSV files and try again.")
      }
      showModal(modalDialog(title = 'Warning',
                            error_modal_body,
                            tags$pre(paste(messages, collapse = '\n')),
                            footer = modalButton('Ok')))
    }
    return(x)
  })

  concepts <- reactive({
    # Set concepts
    if (!is.null(mctd())) {
      x <- setNames(mctd()$AnswerKey$Concept, mctd()$AnswerKey$Question)
      return(x)
    } else {
      return(NULL)
    }
  })

  observe({
    req(concepts())
    x <- concepts()
    updateRadioButtons(session, 'o_overallbox_concepts', choices = c('All', unique(x)), inline = TRUE)
  }, priority = 10)

  observe({
    not_all <- input$o_overallbox_concepts != 'All'
    updateCheckboxInput(session, 'o_overallbox_facet', value = not_all)
  }, priority = 2)

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
    content = function(file) {write.csv(answer_key_example, file, row.names = FALSE)}
  )

  output$down_test_example <- downloadHandler(
    filename = function() {'test_example.csv'},
    content = function(file) {write.csv(test_example, file, row.names = FALSE)}
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

  d_option_pct <- reactive({
    if(is.null(mctd())) return(NULL)
    optionsSelectedPct(mctd(),
                       include_columns = input$o_option_pct_cols,
                       questions_as_row_names = FALSE,
                       as_percentage = input$o_option_pct_count == 'Percentage',
                       correct_vs_incorrect = input$o_option_pct_correct == 'Correct v. Incorrect')
  })

  output$t_option_pct <- DT::renderDataTable(
    DT::datatable(d_option_pct(),
                  filter = 'bottom',
                  rownames = FALSE,
                  options = list(
                    'pageLength' = 10
                  ))
  )

  # ---- Classic Test Theory ----

  summarize_ctt <- reactive({
    if(is.null(mctd())) return(NULL)
    summarizeCTT(mctd(), input$o_classic_summary_table)
  })

  output$t_classic_summary <- DT::renderDataTable(
    DT::datatable(summarize_ctt(),
                  filter = ifelse(input$o_classic_summary_table %in% c('whole', 'concept'), 'none', 'bottom'),
                  autoHideNavigation = TRUE,
                  rownames = FALSE,
                  fillContainer = FALSE,
                  options = list(
                    'pageLength' = 10,
                    # 'autoWidth' = TRUE,
                    'searching' = !(input$o_classic_summary_table %in% c('whole', 'concept')),
                    'paging' = !(input$o_classic_summary_table %in% c('whole', 'concept'))
                  )
    )
  )

  output$txt_classic_summary <- renderUI({
    if (input$o_classic_summary_table == 'whole') {
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
    } else if (input$o_classic_summary_table == 'item') {
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
    } else {
      helpText()
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
    if (input$o_overallbox_concepts == 'All') concepts <- unique(concepts())
    else concepts <- input$o_overallbox_concepts
    testScoreByQuestionPlot(mctd(),
                            concepts = concepts,
                            facet_by_concept = input$o_overallbox_facet)
  })

  recommend_item_actions <- reactive({
    if (is.null(mctd())) return(NULL)
    recommendItemActions(mctd(), input$o_item_review_cols)
  })

  output$t_item_review <- DT::renderDataTable(
    DT::datatable(recommend_item_actions(),
                  filter = 'bottom',
                  autoHideNavigation = TRUE,
                  rownames = FALSE,
                  options = list(
                    'pageLength' = 10
                  )
    )
  )

  output$txt_item_review_help <- renderUI({
    txt <- list()
    txt[['Alpha']] <- tags$p("If Cronbach's Alpha for the test with the item deleted",
                             "is less than the alpha coefficient for the whole test",
                             "then the recommendation is to",
                             tags$strong('Keep'), "the item.")
    txt[['Jorion']] <- tags$p("If the", tags$em('Difficulty Index'),
                              'is between 0.3 and 0.9, and the',
                              tags$em('Discrimination Index'),
                              'is greater than 0.2, then the recommendation is to',
                              tags$strong('Keep'), 'the item.')
    txt[['Versatile']] <- tags$p("This recommendation is based on the",
                                 tags$em("Difficulty Index"), 'and',
                                 tags$em("PBCC"),
                                 "and provides a range of recommendations from",
                                 tags$strong("Remove"), "to",
                                 tags$strong("Review"), "through",
                                 tags$strong("Keep"), ",",
                                 "favoring positive PBCC values near to or greater than 0.3",
                                 "and higher difficulty values."
                                 )
    txt[['Stringent']] <- tags$p("If the", tags$em('Difficulty Index'),
                                 'is between 0.3 and 0.9, and the',
                                 tags$em('point-biserial correlation coefficient'),
                                 'is greater than 0.3, then the recommendation is to',
                                 tags$strong('Keep'), 'the item.')
    return(txt[[input$o_item_review_help_group]])
  })

  # ---- Item Response Theory ----
  output$irt_models_helptext <- renderUI({
    irt_help_text <- list(
      tags$p(
        tags$strong("Difficulty."),
        paste(
          "The difficulty parameter, \\(\\beta\\), sometimes",
          "called the threshold parameter, describes the difficulty of a given item.",
          "It is the only parameter estimated in the 1-PL (Rasch) model.")
      ),
      tags$p(
        tags$strong("Discrimination."),
        paste(
          "The discrimination parameter, \\(\\alpha\\),",
          "reflects the effectiveness of the item in differentiating between high- and",
          "low-performing students. This parameter is estimated in the 2-PL model, in",
          "addition to difficulty.")
      ),
      tags$p(
        tags$strong("Guessing."),
        paste(
          "The guessing parameter, \\(\\gamma\\), is included in the",
          "3-PL model, in addition the previous parameters, and reflects the influence",
          "of guessing for each item.")
      ),
      tags$p(
        tags$strong("Prob."),
        paste("The probability column gives the probability that an average",
              "student will correctly answer the item, i.e.",
              "\\(\\mathrm{P}(x_i = 1 \\vert z = 0)\\).")
      )
    )
    if (input$o_irt_model_summary == 'AIC') {
      helpText('The following table shows the',
               tags$a('Akaike information criterion (AIC)', href = 'https://en.wikipedia.org/wiki/Akaike_information_criterion'),
               'for each of the three models.',
               'In general, a lower AIC value relative to the other model values indicates better model performance.',
               'However, you should be careful to review the model parameter to ensure that model assumptions are valid and appropriate.'
      )
    } else {
      pl_number <- substr(input$o_irt_model_summary, 3, 3)
      withMathJax(helpText(irt_help_text[c(1:pl_number, 4)]))
    }
  })

  output$t_irt_model <- DT::renderDataTable({
    if (is.null(mctd())) return(NULL)
    if (input$o_irt_model_summary == 'AIC') {
      x <- mctd()$irt_models[['AIC']] %>% round(digits = 2)
      x <- data.frame('Model' = names(x), 'AIC' = x)
    } else {
      pl_number <- substr(input$o_irt_model_summary, 3, 3)
      x <- irtSummaryTable(mctd(), pl_number, 'Prob')
    }
    DT::datatable(x,
                  filter = 'bottom',
                  autoHideNavigation = TRUE,
                  rownames = FALSE,
                  options = list(
                    'pageLength' = 10
                  )
    )
  })

  output$p_icc <- renderPlot({
    if (is.null(mctd())) return(NULL)
    if (is.null(input$o_icc_questions)) return(NULL)
    questions <- 1:length(mctd()$AnswerKey$Question)
    names(questions) <- mctd()$AnswerKey$Question
    questions <- questions[input$o_icc_questions]
    switch(input$o_icc_model,
           'PL1' = ltm::plot.rasch(mctd()$irt_models[['PL1']], type = "ICC", items = questions),
           'PL2' = ltm::plot.ltm(mctd()$irt_models[['PL2']], type = "ICC", items = questions),
           'PL3' = ltm::plot.tpm(mctd()$irt_models[['PL3']], type = 'ICC', items = questions)
    )
  })

  ## ICC Curves Form Inputs
  observe({
    if (!is.null(mctd())) {
      x <- mctd()$AnswerKey$Question
      updateCheckboxGroupInput(session, 'o_icc_questions', choices = x, selected = x, inline = TRUE)
    }
  })

  observe({
    if (!is.null(mctd())) {
      x <- unique(concepts())
      updateSelectInput(session, 'o_icc_questions_concept', choices = c('', x))
    }
  })

  observeEvent(input$b_icc_questions_all, {
    if (!is.null(mctd())) {
      x <- mctd()$AnswerKey$Question
      updateCheckboxGroupInput(session, 'o_icc_questions', selected = x)
    }
  })

  observeEvent(input$b_icc_questions_none, {
    updateCheckboxGroupInput(session, 'o_icc_questions', selected = character(0))
  })

  observeEvent(input$b_icc_questions_concept, {
    questions <- concepts()[which(concepts() == input$o_icc_questions_concept)]
    updateCheckboxGroupInput(session, 'o_icc_questions', selected = names(questions))
  })


  # --- Factor Analysis ----
  ## Tetrachoric Plot
  output$p_tetra <- renderPlot({
    if (is.null(mctd())) return(NULL)
    plotTetrachoric(mctd(), input$o_tetra_show_concept, TRUE)
  })

  efa <- reactiveValues('screefactors' = NULL, 'mctd' = NULL)

  output$p_scree <- renderPlot({
    if (is.null(mctd())) return(NULL)
    efa$screefactors <- screePlot(mctd(), TRUE)
  })

  output$txt_scree <- renderUI({
    tagList(
      tags$p(
        "The scree plot shows the eigenvalues of the tetrachoric correlation matrix of the test responses.",
        "Look for a sharp break in the slope of the line between the eigenvalues of the correlation matrix.",
        "In parallel analysis, the scree of factors from the observed data is compared to a random data matrix of the same size as the observed.",
        "Factors from the original data with eigenvalues greater than those of the random data are kept."
      ), if (!is.null(efa$screefactors)) tags$p(
        "Parallel analysis for the test results in this report suggest that the number of factors is",
        tags$strong(efa$screefactors['nfact']),
        "and the number of components is",
        tags$strong(paste0(efa$screefactors['ncomp'], '.')),
        "Note that there are",
        tags$strong(length(unique(mctd()$AnswerKey$Concept))),
        "concept groups in the test design."
      )
    )
  })

  ## EFA
  observeEvent(input$b_run_efa, {
    if (input$o_efa_nfactors == 0) nfactors <- length(unique(concepts()))
    else nfactors <- input$o_efa_nfactors

    efa$mctd <- addEFA(mctd(), nfactors,
                      rotate = input$o_efa_rotate,
                      fm = input$o_efa_fm)
  })

  output$t_efa_out <- renderPrint({
    if (!is.null(efa$mctd)) {
      psych::print.psych(efa$mctd$efa, cut = input$o_efa_cut)
    }
  })

  # output$t_efa_factor_loadings <- DT::renderDataTable({
  #   if (!is.null(efa$mctd)) {
  #     x <- efaTable(efa$mctd, cut = input$o_efa_cut) %>%
  #       mutate_if(is.numeric, round, digits = 2)
  #     if (nrow(x) > 0) {
  #       DT::datatable(x,
  #                     filter = 'bottom',
  #                     autoHideNavigation = TRUE,
  #                     rownames = FALSE,
  #                     options = list(
  #                       'pageLength' = 50
  #                     )
  #       )
  #     } else return(NULL)
  #   }
  # })

  output$t_efa_factor_loadings <- renderTable({
    if (!is.null(efa$mctd)) {
      x <- efaTable(efa$mctd, cut = input$o_efa_cut) %>%
        mutate_if(is.numeric, round, digits = 2)
      if (nrow(x) > 0) x
      else NULL
    }
  }, na = "")

  # ---- Distractor Analysis ----
  distractor.data <- reactive({
    if (is.null(mctd())) return(NULL)
    distractorTable(mctd(), input$o_distractor_pct)
  })

  output$txt_distractor <- renderUI({
    if (is.null(mctd())) return(NULL)

    q1 <- distractor.data()[1, 3]
    distractor.data.counts <- distractor.data() %>%
      filter(Question == q1) %>%
      group_by(Group) %>%
      summarize(total = sum(count))
    distractor.data.counts <- setNames(distractor.data.counts$total,
                                       distractor.data.counts$Group)

    # For turning percentile into words
    percentile_abreviation <- c('th', 'st', 'nd', 'rd', rep('th', 6))
    first_digit <- function(x) round(x %% 10, 0)
    pct_to_text <- function(x) {
      paste0(round(x * 100, 0), percentile_abreviation[first_digit(x*100) + 1])
    }

    tags$p(class = 'help-block',
           "Repondents are grouped into the upper and lower",
           pct_to_text(input$o_distractor_pct),
           "percentiles by overall test score.",
           "For this report, there were",
           distractor.data.counts['high'],
           "respondents in the upper",
           pct_to_text(input$o_distractor_pct), "percentile and",
           distractor.data.counts['low'],
           "repondents in the lower",
           pct_to_text(input$o_distractor_pct), "percentile.",
           "Percentages are calculated relative to the total number of respondents, in this case",
           nrow(mctd()$Test.complete),
           "students."
    )
  })

  output$t_distractor <- DT::renderDataTable({
    if (is.null(mctd())) return(NULL)
    embolden <- function(x, type = 'html') {
      switch(
        type,
        'markdown' = paste0('**', x, '**'),
        'html' = paste0('<strong>', x, '</strong>')
      )
    }

    x <- distractor.data() %>%
      {
        if(input$o_distractor_pct_relative == 'group') mutate(., pct = pct.group)
        else .
      } %>%
      mutate(pct = sprintf("%0.2f", pct*100),
             pct = ifelse(Correct, embolden(pct), pct),
             Group = c('high' = 'H', 'low' = 'L')[Group],
             OptionGroup = paste(Option, Group, sep = '')) %>%
      select(Question, Title, OptionGroup, pct) %>%
      reshape2::dcast(Question + Title ~ OptionGroup, value.var = 'pct')

    x <- suppressWarnings(
      left_join(x,
                mctd()$AnswerKey[, c('Question', "Concept")],
                by = 'Question')
    ) %>% select(Question, Title, Concept, everything())

    DT::datatable(x,
                  filter = 'bottom',
                  autoHideNavigation = TRUE,
                  rownames = FALSE,
                  options = list(
                    'pageLength' = 50
                  ),
                  escape = FALSE
    )
  })

  output$p_distractor <- renderPlot({
    if (is.null(mctd())) return(NULL)
    distractorPlot(
      mctd(),
      pct = input$o_distractor_pct,
      pct_relative = input$o_distractor_pct_relative == 'group',
      use_title = input$o_distractor_show_title
    )
  })
})
