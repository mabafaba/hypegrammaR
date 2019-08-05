#' Map to summary statistic
#'
#' selects an appropriate summary statistic function based on the analysis case
#'
#' @param design the design object (map_to_design())
#' @param dependent.var the name of the dependent variable
#' @param independent.var the name of the independent variable
#' @param case a string uniquely identifying the analysis case. output of map_to_case().
#' @param questionnaire the questionnaire (from load_questionnaire())
#' @param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#' @return a _function_ that computes the relevant summary statistic
#' @examples map_to_summary_statistic("group_difference_categorical_categorical")
#' @examples my_case<- map_to_case( ... )
#' my_sumstat <- map_to_summary_statistic(my_case)
#' my_sumstat( ... )
#' @export
map_to_summary_statistic <-
  function(design,
           dependent.var,
           independent.var,
           case,
           questionnaire = NULL,
           confidence_level = 0.95) {
    # sanitise input:
    if (!is_valid_case_string(case)) {
      stop(
        "'case' input to map_to_summary_statistic must be a valid analysis case; make it with map_to_case()"
      )
    }

    # check if dependent is a select_multiple:
    if (!is.null(questionnaire)) {
      dependent_is_select_multiple <-
        questionnaire$question_is_select_multiple(dependent.var)
      if (dependent_is_select_multiple) {
        dependent.var.sm.cols <-
          questionnaire$choices_for_select_multiple(dependent.var, design$variables)
      }
    } else{
      dependent_is_select_multiple <- FALSE
    }



    if (case == "CASE_group_difference_categorical_categorical") {
      if (dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_multiple_groups(
            dependent.var = dependent.var,
            dependent.var.sm.cols =
              dependent.var.sm.cols,
            independent.var = independent.var,
            design = design
          )
      }
      if (!dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_one_groups(
            dependent.var = dependent.var,
            independent.var = independent.var,
            design = design
          )


      }
    }

    if (case == "CASE_group_difference_numerical_categorical") {
      summary_stat <-
        mean_with_confints_groups(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design,
          confidence_level = confidence_level
        )
    }


    if (case == "CASE_regression_numerical_categorical") {
      summary_stat <-
        mean_with_confints_groups(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design,
          confidence_level = confidence_level
        )
    }

    if (case == "CASE_direct_reporting_numerical_") {
      summary_stat <- mean_with_confints(
        dependent.var = dependent.var,
        design = design,
        confidence_level = confidence_level
      )
    }

    if (case == "CASE_direct_reporting_categorical_") {
      if (dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_multiple(
            dependent.var = dependent.var,
            dependent.var.sm.cols = dependent.var.sm.cols,
            design = design,
            confidence_level = confidence_level
          )
      }


      if (!dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_one(
            dependent.var = dependent.var,
            design = design,
            confidence_level = confidence_level
          )
      }
    }

    if (case == "CASE_direct_reporting_categorical_categorical") {
      if (dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_multiple_groups(
            dependent.var = dependent.var,
            dependent.var.sm.cols =
              dependent.var.sm.cols,
            independent.var = independent.var,
            design = design,
            confidence_level = confidence_level
          )
      }

      if (!dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_one_groups(
            dependent.var = dependent.var,
            independent.var = independent.var,
            design = design,
            confidence_level = confidence_level
          )
      }
    }

    if (case == "CASE_direct_reporting_numerical_categorical") {
      summary_stat <-
        mean_with_confints_groups(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design,
          confidence_level = confidence_level
        )

    }



    if (case == "CASE_limit_categorical") {
      if (dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_multiple(
            dependent.var = dependent.var,
            dependent.var.sm.cols = dependent.var.sm.cols,
            design = design,
            confidence_level = confidence_level
          )
      }


      if (!dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_one(
            dependent.var = dependent.var,
            design = design,
            confidence_level = confidence_level
          )
      }
    }

    if (case == "CASE_limit_numerical") {
      summary_stat <- mean_with_confints(
        dependent.var = dependent.var,
        design = design,
        confidence_level = confidence_level
      )
    }

    if (case == "") {
  stop('case can not be an empty string')

    }


    return(summary_stat)

  }
