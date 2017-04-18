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
#' item analysis results to \link{mcTestAnalysisData} as \code{item.analysis}.
#' @inheritParams mcTestAnalysisData
#' @param ... Additional parameters passed to
#'   \code{\link[psychometric]{item.exam}}
#' @export
addItemAnalysis <- function(mctd, ...) {
  should_have(mctd, 'Test', 'AnswerKey', 'Test.complete')
  mctd <- requires(mctd, 'item.score')
  mctd[['item.analysis']] <- psychometric::item.exam(mctd$item.score, ...)
  return(mctd)
}

#' Add Cronbach Alpha to MC Test Data Object
#'
#' Runs \code{\link[psych]{alpha}} from the \link{psych} package and adds
#' Cronbach alpha and overall student test score results to
#' \link{mcTestAnalysisData} as \code{alpha} and \code{scores}.
#' @inheritParams mcTestAnalysisData
#' @export
addAlpha <- function(mctd) {
  mctd <- requires(mctd, 'item.score')
  mctd[['alpha']] <- psych::alpha(mctd$item.score, warnings = FALSE, check.keys = FALSE)
  mctd[['scores']] <- mctd$alpha$scores
  mctd <- addSubscaleConcept(mctd)
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
addDiscriminationIndex <- function(mctd, percentile = 0.27) {
  mctd <- requires(mctd, c('scores', 'item.score'))
  should_have(mctd, 'Test.complete', 'item.score', 'scores')
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
addPBCC <- function(mctd) {
  mctd <- requires(mctd, c('item.score', 'scores'))
  mctd[['pbcc']] <- cor(mctd$item.score, mctd$scores)[,1]
  return(mctd)
}

#' Modified Point-Biserial Correlation Coefficient
#'
#' Calculates the modified PBCC for each item, where the item answers are
#' correlated with overall test performance without considering the given item
#' in the overall test score.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addPBCCmodified <- function(mctd) {
  mctd <- requires(mctd, 'item.score')
  should_have(mctd, 'AnswerKey', 'item.score')

  raw_test_scores  <- rowSums(mctd$item.score)
  mpbcc        <- rep(NA, length(mctd$AnswerKey$Question))
  names(mpbcc) <- mctd$AnswerKey$Question
  for (j in 1:length(mctd$AnswerKey$Question)) {
    item_scores <- mctd$item.score[, j]
    mpbcc[j] <- cor(item_scores, raw_test_scores - item_scores)
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
  mctd <- requires(mctd, c('discrimination_index', 'pbcc', 'pbcc_modified', 'item.analysis'))
  should_have(mctd, 'Test.complete', 'discrimination_index', 'pbcc', 'pbcc_modified', 'item.analysis')

  type <- tolower(type)
  if (is.null(type)) type <- 'conventional'
  if (type == 'conventional') {
    mctd <- requires(mctd, 'discrimination_index')
    disc <- mctd$discrimination_index
    y_label <- 'Discrimination Index'
  } else if (type == 'pbcc') {
    mctd <- requires(mctd, 'pbcc')
    disc <- mctd$pbcc
    y_label <- 'PBCC'
  } else if (type == 'pbcc_modified') {
    mctd <- requires(mctd, 'pbcc_modified')
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
  mctd <- requires(mctd, c('scores', 'item.score'))
  should_have(mctd, 'item.score', 'scores', 'AnswerKey', 'Test.complete')
  # Prepare Data
  x <- mctd$item.score %>%
    reshape2::melt(variable.name = 'Question', value.name = 'Response', id.vars = character(0)) %>%
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
#' @param summarize_by One of \code{"whole"}, \code{"concept"} or \code{"item"}
#' @param digits.round Round output to specified number of digits, defaults to
#'   \code{digits} option (see \code{getOption("digits")})
#' @export
summarizeCTT <- function(mctd,
                         summarize_by = 'whole',
                         digits.round = getOption('digits')) {
  mctd <- requires(mctd, c('item.score', 'item.analysis', 'alpha',
                            'discrimination_index', 'pbcc', 'pbcc_modified'))
  should_have(mctd, 'AnswerKey', 'alpha', 'item.analysis', 'discrimination_index',
              'pbcc', 'pbcc_modified', 'item.score')
  summarize_by <- tolower(summarize_by)
  if (summarize_by == 'whole') {
    # Overall Test Summary
    # Average of: alpha, difficulty index, discrimination index, item variance, pbcc
    x <- c('Cronbach Alpha'            = mctd$alpha$total$raw_alpha,
           'Avg. Difficulty Index'     = mean(mctd$item.analysis$Difficulty),
           'Avg. Discrimination Index' = mean(mctd$discrimination_index),
           'Avg. PBCC'                 = mean(mctd$pbcc),
           'Avg. Modified PBCC'        = mean(mctd$pbcc_modified),
           'Avg. Item Variance'        = mean(apply(mctd$item.score, 2, sd)^2))
    x <- round(x, digits.round)
    data.frame('Measure' = names(x),
               'Value' = x)
  } else if (summarize_by == 'item') {
    # Individual Item Summary
    # Columns: Question, Title, Concept, Alpha WOI, Difficulty Index,
    #          Discrimination Index, Item Variance, PBCC
    tibble::data_frame('Question'       = mctd$AnswerKey$Question,
                       'Title'          = mctd$AnswerKey$Title,
                       'Concept'        = mctd$AnswerKey$Concept,
                       'Alpha WOI'      = round(mctd$alpha$alpha.drop$raw_alpha, digits.round),
                       'Difficulty'     = round(mctd$item.analysis$Difficulty, digits.round),
                       'Item Var'       = round(apply(mctd$item.score, 2, sd)^2, digits.round),
                       'Discrimination' = round(mctd$discrimination_index, digits.round),
                       'PBCC'           = round(mctd$pbcc, digits.round),
                       'MPBCC'          = round(mctd$pbcc_modified, digits.round)
    )
  } else if (summarize_by == 'concept') {
    mctd <- requires(mctd, 'alpha')
    has_subscale <- !is.null(mctd$alpha$subscale)
    # Same as overall but grouped by concept
    x <- tibble::data_frame('Concept' = unique(mctd$AnswerKey$Concept),
                            'Subscale Alpha'     = NA, #2
                            'Avg Difficulty'     = NA, #3
                            'Avg Discrimination' = NA, #4
                            'Avg PBCC'           = NA, #5
                            'Avg MPBCC'          = NA, #6
                            'Avg Item Var'       = NA) #7
    for (i in 1:nrow(x)) {
      concept <- x$Concept[i]
      questions <- which(mctd$AnswerKey$Concept == concept)
      # Subscale Alpha
      if (has_subscale && !is.null(mctd$alpha$subscale[[concept]])) x[i, 2] <- mctd$alpha$subscale[[concept]]$total$raw_alpha
      # Average Difficulty Index
      if (!is.null(mctd$item.analysis)) x[i, 3] <- mean(mctd$item.analysis$Difficulty[questions])
      # Discrimination Index
      if (!is.null(mctd$discrimination_index)) x[i, 4] <- mean(mctd$discrimination_index[questions])
      # PBCC
      if (!is.null(mctd$pbcc)) x[i, 5] <- mean(mctd$pbcc[questions])
      # MPBCC
      if (!is.null(mctd$pbcc_modified)) x[i, 6] <- mean(mctd$pbcc_modified[questions])
      # Item Variance
      if (!is.null(mctd$item.score)) x[i, 7] <- mean(apply(mctd$item.score[, questions], 2, sd)^2)
    }
    if (!has_subscale) x <- x[, -2]
    x[, 2:ncol(x)] <- try(round(x[, 2:ncol(x)], digits.round))
    return(x)
  } else {
    stop("summarize_by must be one of 'whole', 'concept' or 'item'")
  }
}

#' Calculate Subscale Alpha and Scores
#'
#' Add subscale alpha and CTT reliability analysis to \link{mcTestAnalysisData}.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addSubscaleConcept <- function(mctd) {
  mctd <- requires(mctd, c('alpha', 'item.score'))
  should_have(mctd, 'AnswerKey', 'alpha', 'item.score')
  if (length(unique(mctd$AnswerKey$Concept)) <= 1) {
    warning("Only one concept group found, skipping subscale calculations")
    return(mctd)
  }
  mctd$alpha$subscale <- list()
  for (concept in unique(mctd$AnswerKey$Concept)) {
    questions <- which(mctd$AnswerKey$Concept == concept)
    x <- mctd$item.score[, questions]
    try({
      mctd$alpha$subscale[[concept]] <- psych::alpha(x, warnings = FALSE, check.keys = FALSE)
    }, silent = TRUE)
  }
  return(mctd)
}
