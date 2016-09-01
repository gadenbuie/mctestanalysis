should_have <- function(x, ..., return_missing = FALSE) {
  needed <- c(...)
  doesnt <- needed[!(needed %in% names(x))]
  ismissingsomething <- length(doesnt) > 0
  err <- ''
  if (ismissingsomething) {
    err <- paste(paste(doesnt, collapse = ', '), 'needed but not found in input.')
    if (return_missing) {
      list('has' = !ismissingsomething, 'err' = err)
    } else {
      ## Get name of calling function
      ## http://stackoverflow.com/a/15621405
      calling_function <- deparse(sys.calls()[[max(sys.nframe()-1, 1)]])
      stop(paste0(calling_function, ':\n\t', err), call. = FALSE)
    }
  }
}
