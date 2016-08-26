library(shiny)

ak_file <- system.file("data", 'answer_key_example.csv', package = 'MCTestAnalysis')
if (ak_file != '') {
  answer_key_example <- read.csv(ak_file, stringsAsFactors = FALSE)
}

test_file <- system.file("data", "test_example.csv", package = "MCTestAnalysis")
if (test_file != '') {
  test_example <- read.csv(test_file, stringsAsFactors = FALSE)
}
