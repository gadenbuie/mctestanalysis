#' Option selection per question
#'
#' Returns a dataframe summarizing the percentage of students who chose each of
#' the options availble for each question.
#'
#' @inheritParams mcTestAnalysisData
#' @param include_title Include question title column from Answer Key? (Default:
#'   TRUE)
#' @param questions_as_row_names Move questions column to row name? (Default:
#'   FALSE)
#' @param correct_vs_incorrect Group option into correct option vs incorrect
#'   options? (Default: FALSE)
#' @export
optionsSelectedPct <- function(mctd,
                               include_title = TRUE,
                               questions_as_row_names = FALSE,
                               correct_vs_incorrect = FALSE) {
  needs <- c('Test', 'AnswerKey')
  if (any(needs %in% setdiff(names(mctd), needs))) {
    warning("Test and AnswerKey data needed")
    return(NULL)
  }
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

  # Finalize Format
  if (include_title) {
    x <- x %>%
    left_join(mctd$AnswerKey[, c('Question', 'Title')], by = 'Question') %>%
    select(Question, Title, everything())
  }
  if (questions_as_row_names) x <- tibble::column_to_rownames(x, 'Question')
  return(x)
}
