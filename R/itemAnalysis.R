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
  scores <- rowSums(mctd$item.score)
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
  mctd[['pbcc']] <- cor(mctd$item.score, rowSums(mctd$item.score))[,1]
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
  test_scores  <- rowSums(mctd$item.score)
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

#' Discrimination To Plot Data
#'
#' From Alvaro: We are getting discrimination index with this function. This
#' function will allow you to obtain the discrimination index at the selected
#' percentile once the code runs.
#'
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
    labs(x = "Difficulty Index", y = 'Discrimination Index', color = 'Question')

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
