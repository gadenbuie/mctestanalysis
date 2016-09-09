#' Recommend review actions for each item
#'
#' Recommends review actions for each item based on a set of heuristics or rules
#' based on published guidelines.
#'
#' @inheritParams mcTestAnalysisData
#' @export
recommendItemActions <- function(mctd,
                                 include_columns = c("Title", "Concept", 'Alpha WOI', 'Difficulty', 'Discrimination', 'PBCC'),
                                 digits.round = getOption('digits')) {
  should_have(mctd, 'item.analysis', 'item.score', 'alpha')

  # Check Alpha ----
  # If: Alpha WOI is < overall alpha, then "Keep", else, "Remove"
  check_alpha <- mctd$alpha$alpha.drop$std.alpha < mctd$alpha$total$std.alpha
  check_alpha <- c('Remove', 'Keep')[check_alpha + 1]

  # Check Jorion ----
  # If: Difficulty index between [0.3, 0.9] and discrimination >= 0.2 then "Keep"
  # Else: remove
  check_jorion <- dplyr::between(mctd$item.analysis$Difficulty, 0.3, 0.9)
  check_jorion <- check_jorion & mctd$discrimination_index >= 0.2
  check_jorion <- c('Remove', 'Keep')[check_jorion + 1]

  # Check Versatile ----
  # Using point system:
  # For Difficulty:
  #      +2:  0 <= Diff <= 30
  #      +5: 30 <  Diff <= 50
  #     +10: 50 <  Diff <= 80
  #     +12: 80 <  Diff <= 100
  # For PBCC:
  #      +9: 0.30 <= PBCC
  #      +7: 0.15 <= PBCC < 0.3
  #      +3: 0.00 <= PBCC < 0.15
  #      +0:         PBCC < 0
  # Final recommendation:
  #     Remove:         0 <= score <= 7
  #     Remove/Review:  8 <= score <= 9
  #     Review:        10 <= score <= 12
  #     Review/Keep:         score == 13
  #     Keep (Tough):        score == 14
  #     Keep (Easy):         score == 15
  #     Keep:                score >= 16

  # Levels: (-Inf,0.3] (0.3,0.5] (0.5,0.8] (0.8, Inf]
  diff_scores <- cut(mctd$item.a$Difficulty, breaks = c(-Inf, 0.3, 0.5, 0.8, Inf), labels = c(2,5,10,12))
  diff_scores <- as.integer(paste(diff_scores))
  # Levels: [-Inf,0) [0,0.15) [0.15,0.3) [0.3, Inf)
  pbcc_scores <- cut(mctd$pbcc, breaks = c(-Inf, 0, 0.15, 0.3, Inf), right = FALSE,
                     labels = c(0, 3, 7, 9))
  pbcc_scores <- as.integer(paste(pbcc_scores))

  versatile_scores <- diff_scores + pbcc_scores
  check_versatile  <- cut(versatile_scores,
                          breaks = c(-Inf, 7, 9, 12, 13, 14, 15, Inf),
                          labels = c('Remove', 'Remove/Review', 'Review', 'Review/Keep',
                                     'Keep (Tough)', 'Keep (Easy)', 'Keep'))
  check_versatile <- paste(check_versatile)

  # Check Stringent ----
  # 1. Difficulty index in [0.3, 0.9]
  # 2. PBCC >= 0.3
  # 3. PBCC >= 0.2
  # 4. Discrimination Index >= 0.3
  # 5. Discrimination Index >= 0.2
  # 6. Modified PBCC >= 0.2
  # 7. Alpha_woi <= Alpha_whole
  # check_stringent == 1 & 2
  check_stringent <- dplyr::between(mctd$item.analysis$Difficulty, 0.3, 0.9)
  check_stringent <- check_stringent & mctd$pbcc >= 0.3
  check_stringent <- c('Remove', 'Keep')[check_stringent + 1]

  # Merge into data frame
  x <- tibble::data_frame('Question' = mctd$AnswerKey$Question,
                          'Check Alpha' = check_alpha,
                          'Check Jorion' = check_jorion,
                          'Check Versatile' = check_versatile,
                          'Check Stringent' = check_stringent
  )

  # Finalize Format
  if (!is.null(include_columns)) {
    answer_key_columns <- intersect(include_columns, colnames(mctd$AnswerKey))
    if (any(c('alpha', 'alpha woi') %in% tolower(include_columns))) {
      x <- tibble::add_column(x, 'Alpha WOI' = round(mctd$alpha$alpha.drop$std.alpha, digits.round))
    }
    if (any(c('discrimination', 'discrimination index') %in% tolower(include_columns))) {
      x <- tibble::add_column(x, 'Discrimination' = round(mctd$discrimination_index, digits.round))
    }
    if ('difficulty' %in% tolower(include_columns)) {
      x <- tibble::add_column(x, 'Difficulty' = round(mctd$item.analysis$Difficulty, digits.round))
    }
    if ('pbcc' %in% tolower(include_columns)) {
      x <- tibble::add_column(x, 'PBCC' = round(mctd$pbcc, digits.round))
    }
    if (length(answer_key_columns)){
      x <- x %>%
        left_join(mctd$AnswerKey[, c('Question', answer_key_columns)], by = 'Question')
    }
    non_meta_cols <- setdiff(colnames(x), c('Question', include_columns))
    x <- x[, c('Question', include_columns, non_meta_cols)]
  }
  return(x)
}
