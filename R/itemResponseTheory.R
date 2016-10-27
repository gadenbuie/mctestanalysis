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


#' Plot Tetrachoric Correlations
#'
#' This function is a simple wrapper for the \link[psych]{tetrachoric} function
#' provided by the \code{psych} package.
#'
#' @inheritParams mcTestAnalysisData
#' @param group_by_concept If \code{TRUE}, questions are grouped by concept
#'   group and guidelines and guide text are added to the plot.
#' @param equal_coords If \code{TRUE}, coordinates of the plot are constrained
#'   to be equal (square plot).
#' @export
plotTetrachoric <- function(mctd, group_by_concept = TRUE, equal_coords = FALSE) {
  should_have(mctd, 'item.score', 'AnswerKey')

  if (group_by_concept) {
    concepts <- unique(mctd$AnswerKey$Concept)
    already_grouped <- mctd$AnswerKey %>%
      mutate(Concept.lag = lag(Concept),
             Concept.lag = ifelse(is.na(Concept.lag), 'abcxyz', Concept.lag)) %>%
      filter(Concept != Concept.lag) %>%
      {.$Concept == concepts} %>%
      all()
    ak_ordered <- mctd$AnswerKey %>% mutate(n = 1:nrow(.))
    if (!already_grouped) {
      ak_ordered <- ak_ordered %>% arrange(Concept, n)
      concepts <- unique(ak_ordered$Concept)
    }
    question_order <- ak_ordered$Question
    ak_ordered <- ak_ordered %>% mutate(n = 1:nrow(.))
    group_points <- c()
    for (concept in concepts) {
      max.this.concept <- max(ak_ordered[ak_ordered$Concept == concept, 'n'])
      group_points <- c(group_points, max.this.concept)
    }
  } else {
    question_order <- mctd$AnswerKey$Question
  }

  tetra <- psych::tetrachoric(mctd$item.score)$rho
  tetra <- tetra %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = 'Question') %>%
    reshape2::melt(id.vars = 'Question', variable.name = 'Question2', value.name = 'rho')
  tetra$Question <- factor(tetra$Question, levels = question_order)
  tetra$Question2 <- factor(tetra$Question2, levels = rev(question_order))

  g <- ggplot()+
    geom_raster(data = tetra, aes(x = Question, y = Question2, fill = rho))+
    labs(x = '', y = '', fill = '')+
    theme_minimal()+
    scale_fill_gradient2(limits = c(-1, 1))+
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  if (group_by_concept) {
    N.concept  <- length(unique(mctd$AnswerKey$Concept))
    N.question <- length(unique(mctd$AnswerKey$Question))
    g <- g +
      geom_vline(xintercept = group_points[-N.concept] + 0.5, alpha = 0.3) +
      geom_hline(yintercept = N.question - group_points[-N.concept] + 0.5, alpha = 0.3) +
      geom_text(data = data.frame('Concept' = unique(mctd$AnswerKey$Concept),
                                  'y' = N.question - group_points + 1,
                                  'x' = 1),
                aes(label = Concept, y = y, x = x),
                hjust = 0)
  }
  if (equal_coords) g <- g + coord_equal()
  g
}
