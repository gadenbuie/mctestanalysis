#' Generate Example Test Data
#'
#' This is a quick-and-dirty style exam results generator that can be used to
#' provide, if nothing else, an example test data set that will work throughout
#' all of the MCTestAnalysis functions and interfaces.
#'
#' @param n.students Number of "students"
#' @param n.concepts Number of "concepts" being tested
#' @param n.q_per_concept Number of questions per concept group
#' @param concept_difficulty_min_max Each concept is assigned a difficulty from
#'   a uniform distribution with min and max as defined by this parameter.
#' @param questions_difficulty_mean_sd Each question has a difficult level that
#'   is sampled from a normal distribution with mean and standard deviation as
#'   defined by this parameter.
#' @param students_overall_ability_mean_sd Each student has an overall ability
#'   that is used to scale "his/her" performance across the entire test.
#' @param seed Sets the for this run, for reproducibility
#' @param write_files Should .csv files be written to the working directory?
generateExampleData <- function(
  n.students                       = 100,
  n.concepts                       = 4,
  n.q_per_concept                  = 5,
  concept_difficulty_min_max       = c(0.25, 1),
  questions_difficulty_mean_sd     = c(-0.5, 0.75),
  students_overall_ability_mean_sd = c(0.25, 0.5),
  seed                             = 42,
  write_files                      = TRUE
) {
  set.seed(seed)

  concept_diff  <- setNames(runif(n.concepts,
                                  min = concept_difficulty_min_max[1],
                                  max = concept_difficulty_min_max[2]),
                            LETTERS[1:n.concepts])
  question_diff <- setNames(rnorm(n.questions,
                                  mean = questions_difficulty_mean_sd[1],
                                  sd = questions_difficulty_mean_sd[2]),
                            paste0('Q', 1:n.questions))
  students      <- data.frame('overall_ability' = rnorm(n.students,
                                                        mean = students_overall_ability_mean_sd[1],
                                                        sd = students_overall_ability_mean_sd[2]))
  for (i in 1:n.students) {
    for (concept in LETTERS[1:n.concepts]) {
      students[i, paste0(concept, '_mastery')] <- rnorm(1, mean = students[i, 'overall_ability'], sd=0.05)
    }
  }

  answer <- data.frame(
    'Question' = paste0('Q', 1:n.questions),
    'Answer' = sample(1:4, n.questions, replace = TRUE),
    'Title' = paste("Question", 1:n.questions),
    'Concept' = sort(rep(LETTERS[1:n.concepts], n.q_per_concept))
  )

  possible_answers <- c(1:4)

  score <- matrix(0, n.students, n.questions)
  results <- matrix(-1, n.students, n.questions)
  for (j in 1:n.questions) {
    concept <- answer[j, 'Concept']
    concept_mastery <- paste0(concept, '_mastery')
    correct_response <- answer[j, 'Answer']
    for (i in 1:n.students) {
      z = students[i, 'overall_ability'] + concept_diff[concept] * (students[i, concept_mastery] + question_diff[j])
      if (z == 0) z + 0.001
      pr = 1/(1 + exp(-z))
      score[i, j] <- rbinom(1, 1, pr)
      if (score[i, j] == 0) {
        results[i, j] <- sample(c(rep(possible_answers[-correct_response], 20), -1), 1)
      } else {
        results[i, j] <- correct_response
      }
    }
  }

  results <- as.data.frame(results)
  colnames(results) <- paste0('Q', 1:n.questions)
  results <- mutate_all(results, function(x) ifelse(x<0, NA, x))
  results <- cbind(data.frame(id = paste('Student', 1:n.students)), results)

  if (write_files) {
    write.csv(results, file = 'example_test.csv', row.names = FALSE)
    write.csv(answer, file = 'example_answer_key.csv', row.names = FALSE)
  }
  set.seed(NULL)
  return(list('answer_key' = answer, 'test' = results))
}
