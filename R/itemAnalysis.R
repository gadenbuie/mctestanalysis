#' Add item score to MC Test Data Object
#'
#' Adds a matrix called \code{item.score} to the \link{mcTestAnalysisData}
#' object. Rows are students, columns represent questions and each entry is
#' a binary correct/incorrect response.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addItemScore <- function(mctd) {
  should_have(mctd, 'Test', 'AnswerKey')
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
#' item analysis results to \link{mcTestAnalysisData} as \item{item.analysis}.
#' @inheritParams mcTestAnalysisData
#' @export
addItemAnalysis <- function(mctd) {
  should_have(mctd, 'Test', 'AnswerKey')
  if (!("item.score" %in% names(mctd))) {
    mctd <- addItemScore(mctd)
  }
  mctd[['item.analysis']] <- psychometric::item.exam(mctd$item.score)
  return(mctd)
}

#' Discrimination To Plot Data
#'
#' From Alvaro: We are getting discrimination index with this function. This
#' function will allow you to obtain the discrimination index at the selected
#' percentile once the code runs.
#'
#' @keywords internal
discriminationDifficultyPlot <- function(mctd, percentile = 0.27) {
  if (!('item.analysis' %in% names(mctd))) mctd <- addItemAnalysis(mctd)
  n_items <- ncol(mctd$Test.complete)

  disc <- discrimination_index(mctd)

  x <- data.frame('difficulty' = mctd$item.analysis[, 4],
                  'discrimination' = disc,
                  'question' = names(disc))
  ggplot(x, aes(x = difficulty, y = discrimination, label = question))+
    geom_point()+
    geom_text(nudge_x = -0.025)+
    theme_minimal()+
    labs(x = "Difficulty Index", y = 'Discrimination Index')+
    xlim(0, 1)+
    ylim(0, 1)
}

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
  above <- which(scores >= cutoff[2])
  below <- which(scores <= cutoff[1])
  item_discrimination <- c()
  for (i in 1:n_items) {
    item <- mctd$item.score[, i]
    H <- item[above]
    h <- sum(H)
    H <- length(H)
    L <- item[below]
    l <- sum(L)
    L <- length(L)
    item_discrimination <- c(item_discrimination, h/H - l/L)
  }
  names(item_discrimination) <- colnames(mctd$Test.complete)
  return(item_discrimination)
}
