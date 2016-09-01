#
# This is the server logic of a Shiny web application. This application creates
# the interface for loading your test data and running the mctestanalysis
# package on your testing data.

library(shiny)

shinyServer(function(input, output) {

  # Reactive data elements ----
  mctd <- reactive({
    loadAllData(answer_file = input$f_answer_key$datapath,
                test_file      = input$f_test$datapath,
                has_student_id = input$o_import_has_student_id,
                header         = input$o_import_header,
                sep            = input$o_import_sep,
                quote          = input$o_import_quote)
  })

  # concepts <- reactive({
  #   d_answer_key[, 3]
  # })

  # Import Data Outputs ----

  output$t_answer_key <- renderDataTable({
    if (is.null(input$f_answer_key)) return(NULL)
    mctd()$AnswerKey
  })

  output$t_test <- renderDataTable({
    if (is.null(input$f_test)) return(NULL)
    mctd()$Test
  })

  output$t_data_check <- renderText({
    summary_text <- c()

    add_to_output <- function(...) {
      element <- paste(...)
      summary_text <<- c(summary_text, element)
    }

    is_loaded_answer_key <- 'AnswerKey' %in% names(mctd())
    is_loaded_test <- "Test" %in% names(mctd())
    if (!is_loaded_answer_key) {
      add_to_output('Please upload answer key.')
    }

    if (!is_loaded_test) {
      add_to_output('Please upload test results.')
    }

    if (is_loaded_answer_key & is_loaded_test) {
      add_to_output('Questions:')
      add_to_output('  - In answer key:', length(mctd()$AnswerKey$Question))
      add_to_output('  - In test data: ', ifelse(input$o_import_has_student_id,
                                                 ncol(mctd()$Test[, -1]),
                                                 ncol(mctd()$Test)))
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
})
