#' Add Item Response Theory Model Fits
#'
#' Adds item response theory model fits to the \link{mcTestAnalysisData} object.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addIRTfits <- function(mctd) {
  should_have(mctd, 'item.score')
  irt_models <- list()

  # 1-PL Fit
  data('gh', package = 'ltm')
  irt_models[['PL1']] <- ltm::rasch(mctd$item.score, constraint = cbind(length(mctd$AnswerKey$Question)+1, 1))

  # 2-PL Fit
  irt_models[['PL2']] <- ltm::ltm(mctd$item.score ~ z1)

  # 3-PL Fit
  irt_models[['PL3']] <- ltm::tpm(mctd$item.score, type = 'latent.trait', max.guessing = 1)

  # Calculate AIC for each model
  irt_models[['AIC']] <- unlist(lapply(irt_models, AIC))

  mctd[['irt_models']] <- irt_models
  return(mctd)
}
