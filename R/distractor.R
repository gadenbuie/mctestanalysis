#' Summarize Distractor Analysis
#'
#' Respondents are grouped into High- or Low-performing quantiles according to
#' their overall test score and the quantile given by \code{pct}. If a middle
#' group exists, it is ignored. Note that the two groups may have differing
#' sizes depending on the number of respondents, questions and the distribution
#' of test scores. A table is returned containing a count, within-group
#' percentage and overall percentage of respondents from each of the performance
#' groups who chose each option for all questions.
#'
#' @inheritParams mcTestAnalysisData
#' @param pct Percentage for top/bottom comparison
#' @export
summarizeDistractors <- function(mctd, pct = 0.33) {
  mctd <- requires(mctd, 'scores')
  should_have(mctd, 'AnswerKey', 'Test.complete', 'scores')

  pct <- check_pct(pct)

  cuts <- quantile(mctd$scores, c(pct, 1-pct))
  names(cuts) <- c('low', 'high')
  student_groups <- list(
    'low' = which(mctd$scores <= cuts['low']),
    'high' = which(mctd$scores >= cuts['high'])
  )

  N <- nrow(mctd$Test.complete)
  options <- unique(mctd$AnswerKey$Answer)
  groups <- c('high', 'low')
  questions <- 1:length(mctd$AnswerKey$Question)

  choices <- expand.grid('Option' = options,
                         'Group' = groups,
                         'Question' = questions)
  choices <- arrange(choices, Question, Option, Group)
  choices$Correct <- FALSE
  choices$count <- 0
  choices$pct.group <- NA

  for (q in questions) {
    answers <- mctd$Test.complete[, q]
    for (g in groups) {
      answers.g <- answers[student_groups[[g]]]
      for (o in options) {
        count <- length(answers.g[answers.g == o])
        choices[choices$Option == o &
                  choices$Group == g &
                  choices$Question == q, 'count'] <- count
        answer.correct <- mctd$AnswerKey$Answer[q]
        if (o == answer.correct) {
          choices[choices$Option == o &
                    choices$Group == g &
                    choices$Question == q, 'Correct'] <- TRUE
        }
      }
    }
  }

  for (g in groups) {
    choices[choices$Group == g, 'pct.group'] <-
      choices[choices$Group == g, 'count']/length(student_groups[[g]])
  }
  choices$pct <- choices$count/N

  choices$Question <- factor(choices$Question, labels = mctd$AnswerKey$Question)
  choices <- suppressWarnings(left_join(choices, mctd$AnswerKey[, c('Question', 'Title')], by = 'Question'))
  # Need to refactor Question column, as left_join breaks the factor order
  choices$Question <- factor(choices$Question, levels = mctd$AnswerKey$Question)

  return(choices)
}

#' Plot Distractor Analysis
#'
#' Plots the distractor analysis results as provided by
#' \code{\link{summarizeDistractors}}.
#'
#' @inheritParams summarizeDistractors
#' @param pct_relative Should relative (within-group) percentage or overall
#'   percentage be displayed?
#' @param use_title Should the question title be included in the plot output?
#' @export
plotDistractors <- function(mctd, pct = 0.33, pct_relative = FALSE, use_title = FALSE) {
  pct <- check_pct(pct)
  choices <- summarizeDistractors(mctd, pct)

  choices$Group <- factor(choices$Group,
                          levels = c('low', 'high'),
                          labels = paste(c('Low', 'High'), paste0(round(pct*100, 0), '%')))

  if (pct_relative) choices$pct <- choices$pct.group * 100
  else choices$pct <- choices$pct * 100
  y_lab <- paste('Percentage of', if(pct_relative) "Group's", 'Reponses (%)')

  g <- choices %>%
    ggplot(aes(x = Option, y = pct, alpha = Correct, fill = Group)) +
      geom_bar(stat = 'identity', position = 'dodge') +
      theme_minimal() +
      guides(alpha = FALSE) +
      scale_alpha_discrete(range = c(0.7, 1)) +
      theme(panel.border = element_rect(color = 'grey50', fill=NA),
            strip.background = element_rect(color = 'grey50', fill = 'grey85'),
            legend.position = 'bottom',
            panel.grid.major.x = element_blank(),
            panel.grid.minor.y = element_blank()) +
      labs(x = 'Selected Option', y = y_lab)

  if (use_title) g <- g + facet_wrap(~ paste(Question, Title, sep = ' - '))
  else g <- g + facet_wrap(~ Question)

  return(g)
}


check_pct <- function(pct) {
  if (pct > 1) {
    pct <- pct/100
    warning("Percentage should be in the range [0,1], using ", pct, ' instead.')
  }
  if (pct > 0.5) {
    pct <- 1 - pct
    warning('Supplied percentage was greater than 0.5, using ', pct, ' instead.')
  }
  return(pct)
}
