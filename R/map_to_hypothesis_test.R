
#' map to hypothesis test
#'
#' selects an appropriate hypothesis test function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of \code{\link{map_to_case}}. To list valid case strings use \link{\code{list_all_cases}}
#' @return a _function_ that computes the relevant hypothesis test
#' @examples map_to_summary_statistic("group_difference_categorical_categorical")
#' @examples my_case<- map_to_case( ... )
#' my_hyptest <- map_to_hypothesis_test(my_case)
#' my_hyptest( ... )
#' @export
map_to_hypothesis_test <- function(case) {
  # prefill all valid cases with 'not implemented' errors:
  hypothesis_test_functions<-list()
  hypothesis_test_functions<-lapply(list_all_cases(implemented_only = F),function(x){
    function(...){stop(paste("not implemented: hypothesis test for case",x,".\n the geneva data unit can help!"))}
  })
  names(hypothesis_test_functions)<-list_all_cases(implemented_only = F)

  # add implemented cases:
  hypothesis_test_functions[["CASE_group_difference_categorical_categorical"]] <- hypothesis_test_chisquared
  hypothesis_test_functions[["CASE_direct_reporting_numerical_"]] <- hypothesis_test_empty
  hypothesis_test_functions[["CASE_direct_reporting_categorical_"]] <- hypothesis_test_empty
  hypothesis_test_functions[["CASE_group_difference_numerical_categorical"]] <- hypothesis_test_t_two_sample
  # return function belonging to this case:
  return(hypothesis_test_functions[[case]])
}

