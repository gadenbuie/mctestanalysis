#' Scree Plot
#'
#' Wraps \code{\link[psych]{fa.parallel}} to provide scree plot or automatic
#' determination of appropriate number of components or factors to extract.
#'
#' @inheritParams mcTestAnalysisData
#' @param return_nfactcomp Should suggested number of factors and components be
#'   returned in addition to scree plot?
#' @export
plotScree <- function(mctd, return_nfactcomp = FALSE) {
  mctd <- requires(mctd, 'tetrachoric')
  should_have(mctd, 'tetrachoric', 'Test.complete')

  # psych::fa.parallel uses cat to print a lot of things, and also calls plot by
  # default. There's no way to control this behavior, so the following sends the
  # printout to a temporary file rather than to the console or output.
  tmp <- tempfile()
  sink(tmp)
  on.exit(sink())
  on.exit(file.remove(tmp), add = TRUE)
  x <- psych::fa.parallel(mctd$tetrachoric, n.obs = nrow(mctd$Test.complete))
  # Plot is printed by default
  if (return_nfactcomp) return(c('nfact' = x$nfact, 'ncomp' = x$ncomp))
}


#' Exploratory Factor Analysis
#'
#' Wraps \code{\link[psych]{fa}} function from the \code{\link{psych}} package;
#' see \code{\link[psych]{fa}} for more information.
#'
#' @inheritParams mcTestAnalysisData
#' @inheritParams psych::fa
#' @param ... Passed to \code{\link[psych]{fa}}
#' @export
addEFA <- function(mctd,
                nfactors = length(unique(mctd$AnswerKey$Concept)),
                n.obs = nrow(mctd$Test.complete),
                rotate = 'oblimin',
                fm = 'minres',
                ...) {
  mctd <- requires(mctd, 'tetrachoric')
  should_have(mctd, 'tetrachoric', 'AnswerKey', 'Test.complete')

  mctd[['efa']] <- psych::fa(mctd$tetrachoric, nfactors, n.obs, rotate = rotate, fm = fm, warnings = FALSE, ...)

  return(mctd)
}


#' Summarize Exploratory Factor Analysis
#'
#' Prints a table with EFA factor loadings.
#'
#' @inheritParams addEFA
#' @param cut Suppress factor loadings not greater than this value
#' @param ... Passed to \code{\link{addEFA}}.
#' @export
summarizeEFA <- function(mctd, cut = 0.3, ...) {
  if (!('efa' %in% names(mctd))) {
    mctd <- addEFA(mctd, ...)
  }

  x <- mctd$efa$loadings %>%
    unclass %>%
    reshape2::melt() %>%
    rename(Question = Var1, Factor = Var2) %>%
    mutate(Factor = as.character(Factor),
           Factor = factor(Factor, levels = sort(unique(Factor)))) %>%
    filter(abs(value) >= cut) %>%
    reshape2::dcast(Question ~ Factor) %>%
    full_join(., mctd$AnswerKey[, c('Question', 'Concept')], by = 'Question') %>%
    select(Question, Concept, everything())
  x$Question <- as.character(x$Question)
  x$Question <- factor(x$Question, levels = mctd$AnswerKey$Question)
  x %>% arrange(Question)
}
