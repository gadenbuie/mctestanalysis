#' Scree Plot
#'
#' Wraps \code{\link[psych]{fa.parallel}} to provide scree plot or automatic
#' determination of appropriate number of components or factors to extract.
#'
#' @inheritParams mcTestAnalysisData
#' @param return_nfactcomp Should suggested number of factors and components be
#'   returned in addition to scree plot?
#' @export
screePlot <- function(mctd, return_nfactcomp = FALSE) {
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
