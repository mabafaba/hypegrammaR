

#' map to hypothesis test
#'
#' selects an appropriate hypothesis test function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of map_to_case. To list valid case strings use \link{\code{list_all_cases}}
#' @return a _function_ that computes the relevant hypothesis test
#' @examples map_to_summary_statistic("group_difference_categorical_categorical")
#' @examples my_case<- map_to_case( ... )
#' my_hyptest <- map_to_hypothesis_test(my_case)
#' my_hyptest( ... )
#' @export
map_to_hypothesis_test <- function(design,
                                   dependent.var,
                                   independent.var,
                                   case,
                                   questionnaire = NULL) {
  # prefill all valid cases with 'not implemented' errors:
  hypothesis_test_functions <- list()


  if (is.null(questionnaire)) {
    dependent_is_select_multiple <- FALSE
  }

  if (!is.null(questionnaire)) {
    dependent_is_select_multiple <-
      questionnaire$question_is_select_multiple(dependent.var)
    if (dependent_is_select_multiple) {
      dependent.var.sm.cols <-
        questionnaire$choices_for_select_multiple(dependent.var, design$variables)
    }
  }


  if (case == "CASE_group_difference_categorical_categorical") {
    if (!dependent_is_select_multiple) {
      return(
        hypothesis_test_chisquared_select_one(dependent.var,
                                              independent.var,
                                              design)
      )
    }
    if (dependent_is_select_multiple) {
      return(
        hypothesis_test_chisquared_select_multiple(dependent.var,
                                                   dependent.var.sm.cols = dependent.var.sm.cols,
                                                   independent.var,
                                                   design)
      )
    }
  }

  if (case == "CASE_group_difference_numerical_categorical") {
    return(hypothesis_test_t_two_sample(dependent.var,
                                        independent.var,
                                        design))
  }
  if (case == "CASE_direct_reporting_numerical_") {
    return(hypothesis_test_empty(message = "no hypothesis test on case 'Direct reporting'"))
  }
  if (case == "CASE_direct_reporting_categorical_") {
    return(hypothesis_test_empty(message = "no hypothesis test on case 'Direct reporting'"))

  }
  if (case == " CASE_direct_reporting_categorical_categorical") {
    return(hypothesis_test_empty(message = "no hypothesis test on case 'Direct reporting'"))

  }
  if (case == " CASE_direct_reporting_numerical_categorical") {
    return(hypothesis_test_empty(message = "no hypothesis test on case 'Direct reporting'"))

  }

  return(hypothesis_test_empty(message = "No hypothesis test: case not yet implemented"))
}
