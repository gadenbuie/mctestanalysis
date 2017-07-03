#' Summarize Option Selection per Question
#'
#' Returns a dataframe summarizing the percentage of students who chose each of
#' the options availble for each question.
#'
#' @inheritParams mcTestAnalysisData
#' @param include_columns Vector of column names to include from Answer Key data
#'   (\code{"Answer"}, \code{"Title"}, \code{"Concept"})
#' @param questions_as_row_names Move questions column to row name? (Default:
#'   FALSE)
#' @param as_percentage Summarize by absolute count or percentage of responses
#' @param correct_vs_incorrect Group option into correct option vs incorrect
#'   options? (Default: FALSE)
#' @export
summarizeSelectedOptions <- function(mctd,
                               include_columns = c("Title", "Answer", "Concept"),
                               questions_as_row_names = FALSE,
                               as_percentage = FALSE,
                               correct_vs_incorrect = FALSE) {
  should_have(mctd, 'Test', 'AnswerKey')

  if ('id' %in% names(mctd$Test)) {
    x <- mctd$Test %>% select(-id)
  } else {
    x <- mctd$Test
  }

  # Process Test Data
  x <- x %>%
    reshape2::melt(variable.name = 'Question', value.name = 'Option', id = NULL)
  if (correct_vs_incorrect) {
    x <- x %>%
      mutate(Question = as.character(Question)) %>%
      left_join(mctd$AnswerKey[, c('Question', 'Answer')], by = 'Question') %>%
      mutate(Option = ifelse(Option == Answer, 'Correct', 'Incorrect'))
  }
  x <- x %>%
    group_by(Question, Option) %>%
    tally() %>%
    ungroup() %>%
    mutate(Option = ifelse(is.na(Option), 'Missing', Option)) %>%
    reshape2::dcast(., Question ~ Option, value.var = 'n')
  x[, -1] <- mutate_all(x[, -1], function(x) ifelse(is.na(x), 0, x))
  x$Question <- factor(x$Question, levels = mctd$AnswerKey$Question)
  x <- x[order(x$Question), ]

  if (as_percentage) {
    x[, -1] <- round(x[, -1]/nrow(mctd$Test)*100, 2)
  }

  # Finalize Format
  if (!is.null(include_columns)) {
    x <- x %>%
      left_join(mctd$AnswerKey[, c('Question', include_columns)], by = 'Question')
    non_meta_cols <- setdiff(colnames(x), c('Question', include_columns))
    x <- x[, c('Question', include_columns, non_meta_cols)]
  }
  if (questions_as_row_names) x <- tibble::column_to_rownames(x, 'Question')
  return(x)
}
