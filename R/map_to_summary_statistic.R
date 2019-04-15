#' Map to summary statistic
#'
#' selects an appropriate summary statistic function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of map_to_case().
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
           questionnaire = NULL) {
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
          design = design
        )
    }


    if (case == "CASE_regression_numerical_categorical") {
      summary_stat <-
        mean_with_confints_groups(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design
        )
    }

    if (case == "CASE_direct_reporting_numerical_") {
      summary_stat <- mean_with_confints(
        dependent.var = dependent.var,
        design = design
      )
    }

    if (case == "CASE_direct_reporting_categorical_") {
      if (dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_multiple(
            dependent.var = dependent.var,
            dependent.var.sm.cols = dependent.var.sm.cols,
            design = design
          )
      }


      if (!dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_one(
            dependent.var = dependent.var,
            design = design
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

    if (case == "CASE_direct_reporting_numerical_categorical") {
      summary_stat <-
        mean_with_confints_groups(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design
        )

    }



    if (case == "CASE_limit_categorical_") {
      if (dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_multiple(
            dependent.var = dependent.var,
            dependent.var.sm.cols = dependent.var.sm.cols,
            design = design
          )
      }


      if (!dependent_is_select_multiple) {
        summary_stat <-
          percent_with_confints_select_one(
            dependent.var = dependent.var,
            design = design
          )
      }
    }

    if (case == "CASE_limit_numerical_") {
      summary_stat <- mean_with_confints(
        dependent.var = dependent.var,
        design = design
      )
    }

    if (case == "") {
      preferred_summary_statistic_fun <-
        summary_stat(
          dependent.var = dependent.var,
          independent.var = independent.var,
          design = design
        )
    }


    return(summary_stat)

  }
