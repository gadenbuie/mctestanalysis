#' Add Item Response Theory Model Fits
#'
#' Adds item response theory model fits to an \link{mcTestAnalysisData} object,
#' using the \code{\link{ltm}} package.
#'
#' @section Parameter Description:
#'
#'   \strong{Difficulty.} The difficulty parameter, \eqn{\beta}, sometimes
#'   called the threshold parameter, describes the difficulty of a given item.
#'   It is the only parameter estimated in the 1PL (Rasch) model. Uses
#'   \code{\link[ltm]{rasch}} from \code{ltm}.
#'
#'   \strong{Discrimination.} The discrimination parameter, \eqn{\alpha},
#'   reflects the effectiveness of the item in differentiating between high- and
#'   low-performing students. This parameter is estimated in the 2PL model, in
#'   addition to difficulty. Uses \code{\link[ltm]{ltm}} from \code{ltm}.
#'
#'   \strong{Guessing.} The guessing parameter, \eqn{\gamma}, is included in the
#'   3PL model, in addition the previous parameters, and reflects the influence
#'   of guessing for each item. Uses \code{\link[ltm]{tpm}} from \code{ltm}.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addIRTfits <- function(mctd) {
  mctd <- requires(mctd, 'item.score')
  should_have(mctd, 'item.score', 'AnswerKey')
  irt_models <- list()

  # 1-PL Fit
  if (!requireNamespace('ltm', quietly = TRUE)) {
    stop("The ltm package is required for fitting IRT models.\n",
         "Please install ltm using: install.packages(\"ltm\")")
  }
  tryCatch({
    irt_models[['PL1']] <- ltm::rasch(mctd$item.score, constraint = cbind(length(mctd$AnswerKey$Question)+1, 1))
  },
  error = function(e) {warning(paste("Error fitting PL1:", e$message))})

  # 2-PL Fit
  tryCatch({
    irt_models[['PL2']] <- ltm::ltm(mctd$item.score ~ z1)
  },
  error = function(e) {warning(paste("Error fitting PL2:", e$message))})

  # 3-PL Fit
  tryCatch({
      irt_models[['PL3']] <- ltm::tpm(mctd$item.score, type = 'latent.trait', max.guessing = 1)
  },
  error = function(e) {warning(paste("Error fitting PL3:", e$message))})

  # Calculate AIC for each model
  irt_models[['AIC']] <- unlist(lapply(irt_models, AIC))

  mctd[['irt_models']] <- irt_models
  return(mctd)
}

#' Add Tetrachoric Correlation
#'
#' Adds tetrachoric correlation to the \link{mcTestAnalysisData} object, using
#' \code{\link[psych]{tetrachoric}} from the \link{psych} package.
#'
#' @inheritParams mcTestAnalysisData
#' @export
addTetrachoric <- function(mctd) {
  mctd <- requires(mctd, 'item.score')
  should_have(mctd, 'item.score')
  mctd[['tetrachoric']] <- psych::tetrachoric(mctd$item.score)$rho
  return(mctd)
}


#' Plot Tetrachoric Correlations
#'
#' This function creates a tetrachoric correlation plot using the
#' \link[psych]{tetrachoric} function provided by the \code{psych} package.
#'
#' @inheritParams mcTestAnalysisData
#' @param group_by_concept If \code{TRUE}, questions are grouped by concept
#'   group and guidelines and guide text are added to the plot.
#' @param equal_coords If \code{TRUE}, coordinates of the plot are constrained
#'   to be equal (square plot).
#' @export
plotTetrachoric <- function(mctd, group_by_concept = TRUE, equal_coords = FALSE) {
  mctd <- requires(mctd, c('item.score', 'tetrachoric'))
  should_have(mctd, 'item.score', 'tetrachoric', 'AnswerKey')

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

  tetra <- mctd$tetrachoric %>%
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

#' Summarize IRT Model Results
#'
#' Creates summary table of coefficients for the IRT models.
#'
#' @seealso \code{\link{addIRTfits}}
#'
#' @inheritParams mcTestAnalysisData
#' @param model_params One of \code{1, 2} or \code{3}, indicating number of 1-,
#'   2- or 3-PL model. Can be integer or character.
#' @param probcolname Column name for probability column, defaults to
#'   mathematical expression.
#' @export
summarizeIRT <- function(mctd,
                            model_params = 1,
                            probcolname = '$\\mathrm{P}(x_i = 1 \\vert z = 0)$'){
  mctd <- requires(mctd, 'irt_models')
  should_have(mctd, 'irt_models')
  stopifnot(as.integer(model_params) %in% 1:3)
  if (length(model_params) > 1) {
    model_params <- model_params[1]
    warning("model_params must be only one of 1, 2 or 3, choosing first entry (", model_params, ")")
  }
  pl_name <- paste0('PL', model_params)
  mctd_irt_has <- names(mctd$irt_models)
  if (!(pl_name %in% mctd_irt_has)) {
    warning('No IRT fit for ', pl_name, ' model.')
    return(NULL)
  }
  irt_colnames <- list(
    'PL1' = c('Question', 'Difficulty', 'Discrimination', probcolname),
    'PL2' = c('Question', 'Difficulty', 'Discrimination', probcolname),
    'PL3' = c('Question', 'Guessing', 'Difficulty', 'Discrimination', probcolname)
  )

  irt_summary <- mctd$irt_models[[pl_name]] %>%
    coef(prob = TRUE) %>%
    round(4) %>%
    as.data.frame() %>%
    tibble::rownames_to_column('Question')
  colnames(irt_summary) <- irt_colnames[[pl_name]]

  return(irt_summary)
}
