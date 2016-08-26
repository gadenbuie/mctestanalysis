#
# This is the server logic of a Shiny web application. This application creates
# the interface for loading your test data and running the mctestanalysis
# package on your testing data.

library(shiny)

shinyServer(function(input, output) {

  # Reactive data elements ----

  d_answer_key <- reactive({
    if (is.null(input$f_answer_key)) return(NULL)
    x <- read.csv(input$f_answer_key$datapath,
                  header=input$o_import_header,
                  sep=input$o_import_sep,
                  quote=input$o_import_quote,
                  stringsAsFactors = FALSE)

    expected_columns <- c('Question', 'Answer', 'Title', 'Concept')

    if (ncol(x) > 4) {
      x <- x[, 1:4]
    } else if (ncol(x) == 3) {
      x$Concept <- 'General'
    }

    colnames(x) <- expected_columns

    # Missing concepts given "Missing" concept group
    x[is.na(x$Concept), 'Concept'] <- "Missing"
    return(x)
  })

  d_test <- reactive({
    if (is.null(input$f_test)) return(NULL)
    x <- read.csv(input$f_test$datapath,
                  header=input$o_import_header,
                  sep=input$o_import_sep,
                  quote=input$o_import_quote,
                  stringsAsFactors = FALSE)
    q_index <- ifelse(input$o_import_has_student_id, 2, 1)
    if (input$o_import_has_student_id) {
      # may need to process the test data if first col is ids
      colnames(x)[1] <- 'id'
      x$id <- factor(x$id)
    }
    if (!is.null(d_answer_key())) {
      colnames(x)[q_index:ncol(x)] <- d_answer_key()$Question
    }
    return(x)
  })

  concepts <- reactive({
    d_answer_key[, 3]
  })

  # Import Data Outputs ----

  output$t_answer_key <- renderDataTable({
    if (is.null(input$f_answer_key)) return(NULL)
    d_answer_key()
  })

  output$t_test <- renderDataTable({
    if (is.null(input$f_test)) return(NULL)
    d_test()
  })

  output$t_data_check <- renderText({
    dtest <- d_test()
    dansw <- d_answer_key()
    summary_text <- c()

    add_to_output <- function(...) {
      element <- paste(...)
      summary_text <<- c(summary_text, element)
    }

    is_loaded_answer_key <- !is.null(d_answer_key())
    is_loaded_test <- !is.null(d_test())
    if (!is_loaded_answer_key) {
      add_to_output('Please upload answer key.')
    }

    if (!is_loaded_test) {
      add_to_output('Please upload test results.')
    }

    if (is_loaded_answer_key & is_loaded_test) {
      add_to_output('Questions:')
      add_to_output('  - In answer key:', length(dansw$Question))
      add_to_output('  - In test data: ', ifelse(input$o_import_has_student_id, ncol(dtest[, -1]), ncol(dtest)))
      add_to_output('')
      add_to_output('Responses:')
      add_to_output('  - Incomplete:', nrow(dtest[!complete.cases(dtest),]))
      add_to_output('  - Total:', nrow(dtest))
      add_to_output('')
      add_to_output('Concepts:', length(unique(dansw$Concept)))
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
