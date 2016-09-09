#' Add item score to MC Test Data Object
#'
#' Adds a matrix called \code{item.score} to the \link{mcTestAnalysisData}
#' object. Rows are students, columns represent questions and each entry is
#' a binary correct/incorrect response.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addItemScore <- function(mctd) {
  should_have(mctd, 'Test', 'AnswerKey', 'Test.complete')
  x <- apply(mctd$Test.complete, 1,
             function(x) as.integer(x == mctd$AnswerKey$Answer))
  x <- as.data.frame(t(x))
  names(x) <- names(mctd$Test.complete)
  mctd[['item.score']] <- x
  return(mctd)
}

#' Add item analysis to MC Test Data Object
#'
#' Runs \code{\link{item.exam}} from the \link{psychometric} package and adds
#' item analysis results to \link{mcTestAnalysisData} as \link{item.analysis}.
#' @inheritParams mcTestAnalysisData
#' @param ... Additional parameters passed to
#'   \code{\link[psychometric]{item.exam}}
#' @export
addItemAnalysis <- function(mctd, ...) {
  should_have(mctd, 'Test', 'AnswerKey', 'Test.complete')
  if (!("item.score" %in% names(mctd))) {
    mctd <- addItemScore(mctd)
  }
  mctd[['item.analysis']] <- psychometric::item.exam(mctd$item.score, ...)
  return(mctd)
}


#' Calculate Conventional Item Discrimination Index
#'
#' Calculates the conventional item discrimination index, based on complete test
#' results only. Follows the formulation described in Lopez (1998), where item
#' discrimination \eqn{d = p(UG) - p(LG)}. UG and LG are the upper and lower
#' groups -- the upper and lower 27\% of students as ranked by their overall
#' test scores -- and \eqn{p(UG)} and \eqn{p(LG)} are the proportion of correct
#' answers in each group.
#'
#' @references \emph{The Item Discrimination Index: Does it Work?} Tristan Lopez
#'   A. Rasch Measurement Transactions, 1998, 12:1 p. 626.
#'   \url{http://www.rasch.org/rmt/rmt121r.htm}
#'
#' @inheritParams mcTestAnalysisData
#' @param percentile Percentile defining upper and lower groups (standard is
#'   0.27)
#' @export
discriminationIndex <- function(mctd, percentile = 0.27) {
  if (!("item.score" %in% names(mctd))) mctd <- addItemScore(mctd)
  n_students <- nrow(mctd$Test.complete)
  n_items <- ncol(mctd$Test.complete)

  # item discrimination index is (h/H - l/L)
  # h = # of students in upper percentile correct on item
  # H = # of students in upper percentile
  # l = # of students in lower percentile correct on item
  # L = # of students in lower percentile
  scores <- mctd$scores
  cutoff <- quantile(scores, c(percentile, 1 - percentile))
  upper <- which(scores >= cutoff[2])
  lower <- which(scores <= cutoff[1])
  item_discrimination <- c()
  for (i in 1:n_items) {
    item <- mctd$item.score[, i]
    H <- item[upper]
    h <- sum(H)
    H <- length(H)
    L <- item[lower]
    l <- sum(L)
    L <- length(L)
    item_discrimination <- c(item_discrimination, h/H - l/L)
  }
  names(item_discrimination) <- colnames(mctd$Test.complete)
  mctd[['discrimination_index']] <- item_discrimination
  return(mctd)
}

#' Point-Biserial Correlation Coefficient
#'
#' Calculates standard PBCC (or Pearson-moment correlation), in which the
#' answers to each item are correlated with overall test performance.
#'
#' @inheritParams mcTestAnalysisData
#' @export
pbcc <- function(mctd) {
  if (!("item.score" %in% names(mctd))) mctd <- addItemScore(mctd)
  mctd[['pbcc']] <- cor(mctd$item.score, mctd$scores)[,1]
  return(mctd)
}

#' Modified Point-Biserial Correlation Coefficient
#'
#' Calculates the modified PBCC for each item.
#'
#' @inheritParams mcTestAnalysisData
#' @export
pbcc_modified <- function(mctd) {
  if (!("item.score" %in% names(mctd))) mctd <- addItemScore(mctd)
  test_scores  <- mctd$scores
  N            <- nrow(mctd$Test.complete)
  mpbcc        <- rep(NA, length(mctd$AnswerKey$Question))
  names(mpbcc) <- mctd$AnswerKey$Question
  for (j in 1:length(mctd$AnswerKey$Question)) {
    item_scores <- mctd$item.score[, j]
    mpbcc[j] <- (N * item_scores %*% test_scores - sum(item_scores)*sum(test_scores)) /
      (sqrt((N * item_scores %*% item_scores - sum(item_scores)^2)*
              (N * test_scores %*% test_scores - sum(test_scores)^2)))
  }
  mctd[['pbcc_modified']] <- mpbcc
  return(mctd)
}

#' Plot Discrimination Index vs Difficulty Index
#'
#' Plots discrimination index, as calculated using the \code{"conventional"}
#' formula, or via \code{"PBCC"}, against difficulty index. Several parameters
#' control settings related to the appearance of the final plot.
#'
#' @inheritParams mcTestAnalysisData
#' @param type One of \code{"conventional"}, \code{"pbcc"}, or
#'   \code{"pbcc_modified"}
#' @param show_labels Should the question number be shown next to points?
#' @param hide_legend Should the plot legend be hidden?
#' @param show_guidelines Should recomended discrimination or difficulty indice
#'   ranges be shown on the plot as a dotted line?
#' @param max_limits Set x- and y-axis limits. One of \code{"max_x"} (x
#'   positive, y free), \code{"max_y"} (x free, y positive), \code{"max_y+"} (x
#'   free, y in [-1,1]), \code{"max_all+"} (x positive, y positive),
#'   \code{"max_all"} (x positive, y in [-1,1]), and \code{NULL} (all free).
#' @export
discriminationDifficultyPlot <- function(mctd,
                                         type = 'conventional',
                                         show_labels = TRUE,
                                         hide_legend = TRUE,
                                         show_guidelines = TRUE,
                                         max_limits = 'max_x') {
  if (!('item.analysis' %in% names(mctd))) mctd <- addItemAnalysis(mctd)
  type <- tolower(type)
  if (is.null(type)) type <- 'conventional'
  if (type == 'conventional') {
    if (!('discrimination_index' %in% names(mctd))) mctd <- discriminationIndex(mctd)
    disc <- mctd$discrimination_index
    y_label <- 'Discrimination Index'
  } else if (type == 'pbcc') {
    if (!('pbcc' %in% names(mctd))) mctd <- pbcc(mctd)
    disc <- mctd$pbcc
    y_label <- 'PBCC'
  } else if (type == 'pbcc_modified') {
    if (!('pbcc_modified' %in% names(mctd))) mctd <- pbcc_modified(mctd)
    disc <- mctd$pbcc_modified
    y_label <- 'Modified PBCC'
  } else {
    stop(paste('Unknown discrimination index type', type))
  }
  n_items <- ncol(mctd$Test.complete)

  x <- data.frame('difficulty' = mctd$item.analysis[, 'Difficulty'],
                  'discrimination' = disc,
                  'question' = names(disc))
  x$question <- factor(x$question, levels = colnames(mctd$Test.complete))
  g <- ggplot(x, aes(x = difficulty, y = discrimination, label = question, color = question))+
    geom_point()+
    theme_minimal()+
    labs(x = "Difficulty Index", y = y_label, color = 'Question')

  if (show_labels) {
    g <- g + geom_text(nudge_x = -0.025, show.legend = FALSE)
  }
  if (hide_legend) {
    g <- g + guides(color = FALSE)
  }
  if (show_guidelines) {
    g <- g +
      geom_vline(xintercept = 0.2, linetype = 'dashed', color = 'grey80')+
      geom_vline(xintercept = 0.8, linetype = 'dashed', color = 'grey80')+
      geom_hline(yintercept = 0.2, linetype = 'dashed', color = 'grey80')
  }
  if (!is.null(max_limits)) {
    g <- switch(max_limits,
                'max_y' = g + ylim(-1, 1),
                'max_y+' = g + ylim(0, 1),
                'max_x' = g + xlim(0, 1),
                'max_all' = g + ylim(-1, 1) + xlim(0, 1),
                'max_all+' = g + ylim(0, 1) + xlim(0, 1)
                )
  }
  return(g)
}


#' Plot Overall Test Score by Question and Correctness
#'
#' Boxplot of test scores compared with question responses.
#'
#' @inheritParams mcTestAnalysisData
#' @param concepts Character vector containing concept groups to be plotted
#' @param facet_by_concept Should plot be facetted by concept group?
#' @export
testScoreByQuestionPlot <- function(mctd,
                                    concepts = unique(mctd$AnswerKey$Concept),
                                    facet_by_concept = FALSE) {
  if (!('item.score' %in% names(mctd))) mctd <- addItemScore(mctd)
  # Prepare Data
  x <- mctd$item.score %>%
    reshape2::melt(variable.name = 'Question', value.name = 'Response') %>%
    left_join(mctd$AnswerKey[, c('Question', 'Concept')], by = 'Question') %>%
    mutate(Score = rep(mctd$scores, nrow(mctd$AnswerKey)),
           Response = ifelse(Response, 'Correct', 'Incorrect'),
           Response = factor(Response, levels = c('Incorrect', 'Correct'))) %>%
    filter(Concept %in% concepts)
  x$Question <- factor(x$Question, levels = colnames(mctd$Test.complete))

  # Prepare Plot
  g <- x %>%
    ggplot(aes(x = Question, y = Score, fill = Response))+
    geom_boxplot()+
    theme_minimal()+
    ylim(0, 1)+
    labs(x = '', y = 'Overall Test Score', fill = '')+
    theme(legend.position = 'bottom')
  if (facet_by_concept) {
    concept_title <- function(string) {
      paste("Concept:", string)
    }
    g <- g +
      facet_wrap(~ Concept, scales = 'free_x', labeller = labeller(Concept = concept_title)) +
      theme(panel.border = element_rect(color = 'grey50', fill=NA))
  }
  return(g)
}


#' Generate Classic Test Theory Results Summary Table
#'
#' Summarizes Classic Test Theory results for the loaded test.
#'
#' @inheritParams mcTestAnalysisData
#' @param overall_summary Whole test summary (TRUE) or individual item summary
#'   (FALSE)?
#' @export
summarizeCTT <- function(mctd,
                         overall_summary = TRUE) {
  should_have(mctd, 'alpha', 'discrimination_index', 'pbcc')
  if (overall_summary) {
    # Overall Test Summary
    # Average of: alpha, difficulty index, discrimination index, item variance, pbcc
    x <- c('Cronbach Alpha'       = mctd$alpha$total$std.alpha,
           'Difficulty Index'     = mean(mctd$item.analysis$Difficulty),
           'Discrimination Index' = mean(mctd$discrimination_index),
           'PBCC'                 = mean(mctd$pbcc),
           'Item Variance'        = mean(apply(mctd$item.score, 2, sd)^2))
    x <- round(x, 2)
    data.frame('Measure' = names(x),
               'Average' = x)
  } else {
    # Individual Item Summary
    # Columns: Question, Title, Concept, Alpha WOI, Difficulty Index,
    #          Discrimination Index, Item Variance, PBCC
    tibble::data_frame('Question'       = mctd$AnswerKey$Question,
                       'Title'          = mctd$AnswerKey$Title,
                       'Concept'        = mctd$AnswerKey$Concept,
                       'Alpha WOI'      = round(mctd$alpha$alpha.drop$std.alpha, 2),
                       'Difficulty'     = round(mctd$item.analysis$Difficulty, 2),
                       'Item Var'       = round(apply(mctd$item.score, 2, sd)^2, 2),
                       'Discrimination' = round(mctd$discrimination_index, 2),
                       'PBCC'           = round(mctd$pbcc, 2)
    )

  }
}
