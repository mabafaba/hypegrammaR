#'Weighted percentages with confidence intervals
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a 'select one'
#'@param independent.var should be null ! For other functions: string with the column name in `data` of the independent variable
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details this function takes the design object and the name of your dependent variable when this one is a select one. It calculates the weighted percentage for each category.
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value, independent.var, independent.var.value, numbers, se, min and max.
#'@examples percent_with_confints_select_one("population_group", design)
#'@export
percent_with_confints_select_one <-
  function(dependent.var,
           independent.var = NULL,
           design,
           na.rm = TRUE,
           confidence_level = 0.95) {
    if (!is.null(independent.var)) {
      warning(
        "confidence intervals calculated without disaggregation, but received data for an independent variable."
      )
    }

    stopifnot(is.numeric(confidence_level))
    sanitised<-datasanitation_design(design,dependent.var,independent.var = NULL,
                                     datasanitation_summary_statistics_percent_with_confints_select_one)

    if(!sanitised$success){
      warning(sanitised$message)
      return(datasanitation_return_empty_table(data = design$variables, dependent.var, independent.var))}

    design<-sanitised$design
    tryCatch(
      expr = {
        result_hg_format <-
        {
          design$variables[[dependent.var]] <-
            as.factor(design$variables[[dependent.var]])

          result_svy_format <-
            svymean(formula(paste0("~", dependent.var)), design, level = confidence_level) %>% cbind(., confint(.,level = confidence_level))
          colnames(result_svy_format) <- c("numbers", "min", "max")
          summary_with_confints <- data.frame(
            dependent.var = dependent.var,
            independent.var = NA,
            dependent.var.value = gsub(
              paste0("^", dependent.var),
              "",
              rownames(result_svy_format)
            ),
            independent.var.value = NA,
            numbers = result_svy_format[, "numbers"],
            se = NA,
            min = result_svy_format[, "min"],
            max = result_svy_format[, "max"]
          )
          summary_with_confints[, "min"] <-
            summary_with_confints[, "min"] %>% replace(summary_with_confints[, "min"] < 0 , 0)
          summary_with_confints[, "max"] <-
            summary_with_confints[, "max"] %>% replace(summary_with_confints[, "max"] > 1 , 1)
          summary_with_confints %>% as.data.frame
        }
        return(result_hg_format)
      },
      error = function(e) {
        .write_to_log("percent_with_confints_select_one failed with error:")
        .write_to_log(e$message)
      }
    )
  }

#'Weighted percentages with confidence intervals for select multiple questions
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a 'select multiple.
#'@param dependent.var.sm.cols a vector with the columns indices of the choices for the select multiple question. Can be obtained by calling choices_for_select_multiple(question.name, data)
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details this function takes the design object and the name of your dependent variable when this one is a select multiple. It calculates the weighted percentage for each category.
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value, independent.var (= NA), independent.var.value (= NA), numbers, se, min and max.
#'@export
percent_with_confints_select_multiple <- function(dependent.var,
                                                  dependent.var.sm.cols,
                                                  design,
                                                  na.rm = TRUE,
                                                  confidence_level = 0.95) {


  stopifnot(is.numeric(confidence_level))

  question_matches_choices(design$variables, dependent.var, sm.columns = dependent.var.sm.cols)


  ### Sanitation checks
  sapply(dependent.var.sm.cols, function(x){

    dependent.var <- names(design$variables)[x]
    sanitised<-datasanitation_design(design,dependent.var,independent.var = NULL,
                                     datasanitation_summary_statistics_percent_sm_choice)
    if(!sanitised$success){
      warning(sanitised$message)
      return(datasanitation_return_empty_table(data = design$variables, dependent.var))
      }
    design<-sanitised$design
    }
    )
  ###

  # Get the columns with the choices data into an object
  choices <- design$variables[, dependent.var.sm.cols]


  result_hg_format <- lapply(names(choices), function(x) {
    design$variables[[x]] <- as.logical(design$variables[[x]])
    result_svy_format <-
      svymean(formula(paste0("~", x)), design, level = confidence_level) %>% cbind(., confint(.,level = confidence_level))
    result_svy_format <-
      result_svy_format[rownames(result_svy_format) == paste0(x, TRUE), , drop =
                          F]
    colnames(result_svy_format) <- c("numbers", "min", "max")
    if (nrow(result_svy_format) > 0) {
      summary_with_confints <- data.frame(
        dependent.var = dependent.var,
        independent.var = NA,
        dependent.var.value = gsub(paste0("^", dependent.var, "."), "", x),
        independent.var.value = NA,
        numbers = result_svy_format[, "numbers"],
        se = NA,
        min = result_svy_format[, "min"],
        max = result_svy_format[, "max"]
      )
    } else{
      summary_with_confints <- data.frame(
        dependent.var = dependent.var,
        independent.var = NA,
        dependent.var.value = gsub(paste0("^", dependent.var, "."), "", x),
        independent.var.value = NA,
        numbers = NA,
        se = NA,
        min = NA,
        max = NA
      )
    }

  })
  result_hg_format %<>% do.call(rbind, .)

  result_hg_format[, "min"] <-
    result_hg_format[, "min"] %>% replace(result_hg_format[, "min"] < 0 , 0)
  result_hg_format[, "max"] <-
    result_hg_format[, "max"] %>% replace(result_hg_format[, "max"] > 1 , 1)
  result_hg_format %>% as.data.frame

  return(result_hg_format)
}



#'Weighted percentages with confidence intervals for groups
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a 'select one'
#'@param independent.var string with the column name in `data` of the independent (group) variable. Should be a 'select one'
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details this function takes the design object and the name of your dependent variable when this one is a select one. It calculates the weighted percentage for each category in each group of the independent variable.
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value, independent.var, independent.var.value, numbers, se, min and max.
#'@examples percent_with_confints_select_one_groups("population_group", "resp_gender", design)
#'@export
percent_with_confints_select_one_groups <- function(dependent.var,
                                                    independent.var,
                                                    design,
                                                    na.rm = TRUE,
                                                    confidence_level = 0.95) {

  stopifnot(is.numeric(confidence_level))

  sanitised<-datasanitation_design(design,dependent.var,independent.var,
                                   datasanitation_summary_statistics_percent_groups)
  if(!sanitised$success){
    warning(sanitised$message)
    return(datasanitation_return_empty_table(data = design$variables, dependent.var,independent.var))}

  design<-sanitised$design

  formula_string <- paste0("~", dependent.var , sep = "")
  by <- paste0("~", independent.var , sep = "")


  result_hg_format <- {
    design$variables[[dependent.var]] <-
      as.factor(design$variables[[dependent.var]])

    result_svy_format <-
      svyby(
        formula(formula_string),
        formula(by),
        design,
        svymean,
        na.rm = T,
        keep.var = T,
        vartype = "ci",
        level = confidence_level
      )

    unique.dependent.var.values <-
      design$variables[[dependent.var]] %>% unique
    summary_with_confints <- unique.dependent.var.values %>%
      lapply(function(x) {
        summary_stat_colname <- paste0(dependent.var, x)
        lower_confint_colname <- paste0("ci_l.", summary_stat_colname)
        upper_confint_colname <- paste0("ci_u.", summary_stat_colname)

        dependent_value_x_stats <-
          result_svy_format[, c(
            independent.var,
            summary_stat_colname,
            lower_confint_colname,
            upper_confint_colname
          )]
        colnames(dependent_value_x_stats) <-
          c("independent.var.value", "numbers", "min", "max")
        data.frame(
          dependent.var = dependent.var,
          independent.var = independent.var,
          dependent.var.value = x,
          independent.var.value = dependent_value_x_stats[, "independent.var.value"],
          numbers = dependent_value_x_stats[, "numbers"],
          se = NA,
          min = dependent_value_x_stats[, "min"],
          max = dependent_value_x_stats[, "max"]
        )
      }) %>% do.call(rbind, .)


    summary_with_confints[, "min"] <-
      summary_with_confints[, "min"] %>% replace(summary_with_confints[, "min"] < 0 , 0)
    summary_with_confints[, "max"] <-
      summary_with_confints[, "max"] %>% replace(summary_with_confints[, "max"] > 1 , 1)
    summary_with_confints %>% as.data.frame
  }

  return(result_hg_format)
}

#'Weighted percentages with confidence intervals for groups (select multiple questions)
#'
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a 'select multiple.
#'@param dependent.var.sm.cols a vector with the columns indices of the choices for the select multiple question. Can be obtained by calling choices_for_Select_multiple(question.name, data)
#'@param independent.var string with the column name in `data` of the independent (group) variable. Should be a 'select one'
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details this function takes the design object and the name of your dependent variable when this one is a select multiple. It calculates the weighted percentage for each category.
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value, independent.var (= NA), independent.var.value (= NA), numbers, se, min and max.
#'@export
#'
percent_with_confints_select_multiple_groups <-
  function(dependent.var,
           dependent.var.sm.cols,
           independent.var,
           design,
           na.rm = TRUE,
           confidence_level = 0.95) {

    stopifnot(is.numeric(confidence_level))

    # if dependent and independent variables have only one value, just return that:

    question_matches_choices(design$variables, dependent.var, sm.columns = dependent.var.sm.cols)

    ### Sanitation checks
    sapply(dependent.var.sm.cols, function(x){

      dependent.var <- names(design$variables)[x]
      sanitised<-datasanitation_design(design,dependent.var,independent.var,
                                       datasanitation_summary_statistics_percent_sm_choice_groups)
      if(!sanitised$success){
        warning(sanitised$message)
        return(datasanitation_return_empty_table_NA(data = design$variables, dependent.var, independent.var)) ### hack because in the etch case of a numeric dependent the whole thing goes
      }
      design<-sanitised$design
    })

    ###
    # Get the columns with the choices data into an object
    choices <- design$variables[, dependent.var.sm.cols]


    result_hg_format <- lapply(names(choices), function(x){
      formula_string_sans_tilde <- paste0("as.numeric(", x , ")", sep = "")
      formula_string <- paste0("~as.numeric(", x , ")", sep = "")
      by <- paste0("~", independent.var , sep = "")

      result_svy_format <-
        svyby(
          formula(formula_string),
          formula(by),
          design,
          svymean,
          na.rm = T,
          keep.var = T,
          vartype = "ci",
          level = confidence_level
        )

      summary_stat_colname <- formula_string_sans_tilde
      lower_confint_colname <- paste0("ci_l")
      upper_confint_colname <- paste0("ci_u")

      dependent_value_x_stats <-
        result_svy_format[, c(
          independent.var,
          summary_stat_colname,
          lower_confint_colname,
          upper_confint_colname
        )]
      colnames(dependent_value_x_stats) <-
        c("independent.var.value", "numbers", "min", "max")
      data.frame(
        dependent.var = dependent.var,
        independent.var = independent.var,
        dependent.var.value = gsub(paste0("^", dependent.var, "."), "", x),
        independent.var.value = dependent_value_x_stats[, "independent.var.value"],
        numbers = dependent_value_x_stats[, "numbers"],
        se = NA,
        min = dependent_value_x_stats[, "min"],
        max = dependent_value_x_stats[, "max"]
      )
    })

    result_hg_format %<>% do.call(rbind, .)

    result_hg_format[, "min"] <-
      result_hg_format[, "min"] %>% replace(result_hg_format[, "min"] < 0 , 0)
    result_hg_format[, "max"] <-
      result_hg_format[, "max"] %>% replace(result_hg_format[, "max"] > 1 , 1)
    result_hg_format %>% as.data.frame


    return(result_hg_format)
  }



#'Weighted means with confidence intervals
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a numerical variable.
#'@param independent.var should be null ! For other functions: string with the column name in `data` of the independent variable
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details This function takes the design object and the name of your dependent variable when the latter is a numerical. It calculates the weighted mean for your variable.
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value (=NA), independent.var (= NA), independent.var.value (= NA), numbers (= mean), se, min and max.
#'@export
mean_with_confints <- function(dependent.var,
                               independent.var = NULL,
                               design,
                               confidence_level = 0.95) {

  stopifnot(is.numeric(confidence_level))

  if (!is.null(independent.var)) {
    warning(
      "confidence intervals calculated without disaggregation, but received data for an independent variable."
    )
  }

  sanitised<-datasanitation_design(design,dependent.var,independent.var = NULL,
                                   datasanitation_summary_statistics_mean)
  if(!sanitised$success){
    warning(sanitised$message)
    return(datasanitation_return_empty_table(design$variables, dependent.var))}

  datasanitation_summary_statistics_mean
  formula_string <- paste0("~as.numeric(", dependent.var, ")")
  summary <- svymean(formula(formula_string),
                     design,
                     na.rm = T)

  confints <- confint(summary, level = confidence_level)
  results <- data.frame(
    dependent.var = dependent.var,
    independent.var = "NA",
    dependent.var.value = "NA",
    independent.var.value = "NA",
    numbers = summary[1],
    se = summary[2],
    min = confints[1],
    max = confints[2]
  )
  return(results)
}

#'Weighted means with confidence intervals for groups
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a numerical variable.
#'@param independent.var string with the column name in `data` of the independent (group) variable. Should be a 'select one'
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details This function takes the design object and the name of your dependent variable when the latter is a numerical. It calculates the weighted mean for your variable.
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value (=NA), independent.var, independent.var.value, numbers (= mean), se, min and max.
#'@export
mean_with_confints_groups <- function(dependent.var,
                                      independent.var,
                                      design,
                                      confidence_level = 0.95) {

  sanitised <-datasanitation_design(design,dependent.var,independent.var,
                                   datasanitation_summary_statistics_mean_groups)
  if(!sanitised$success){
    warning(sanitised$message)
    return(datasanitation_return_empty_table_NA(design$variables, dependent.var, independent.var))}

  formula_string <- paste0("~as.numeric(", dependent.var, ")")
  by <- paste0("~", independent.var, sep = "")


  result_svy_format <-
    svyby(
      formula(formula_string),
      formula(by),
      design,
      svymean,
      na.rm = T,
      keep.var = T,
      vartype = "ci",
      level = confidence_level
    )
  unique.independent.var.values <-
    design$variables[[independent.var]] %>% unique
  results <- unique.independent.var.values %>%
    lapply(function(x) {
      dependent_value_x_stats <- result_svy_format[as.character(x), ]
      colnames(dependent_value_x_stats) <-
        c("independent.var.value", "numbers", "min", "max")
      data.frame(
        dependent.var = dependent.var,
        independent.var = independent.var,
        dependent.var.value = NA,
        independent.var.value = x,
        numbers = dependent_value_x_stats[2],
        se = NA,
        min = dependent_value_x_stats[3],
        max = dependent_value_x_stats[4]
      )
    }) %>% do.call(rbind, .)

  return(results)
}

### for select_one and select multiple answers, returns the most common answer for that group
# only works for select_one and select_multiple

#'Weighted means with confidence intervals for groups
#'@param dependent.var string with the column name in `data` of the dependent variable. Should be a select_one or a select_multiple.
#'@param independent.var string with the column name in `data` of the independent (group) variable. Should be a 'select one'
#'@param design the svy design object created using map_to_design or directly with svydesign
#'@param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#'@details This function takes the design object and the name of your dependent variable, and returns the most frequent answer for each category in independent.var
#'@return A table in long format of the results, with the column names dependent.var, dependent.var.value (=NA), independent.var, independent.var.value, numbers (= mean), se, min and max.
#'@export
summary_statistic_mode_select_one <-
  function(dependent.var, independent.var, design, confidence_level = 0.95) {
    percent <-
      percent_with_confints_select_one_groups(dependent.var, independent.var, design,confidence_level = confidence_level)
    modes <-
      percent %>% split.data.frame(percent$independent.var.value, drop = T) %>% lapply(function(x) {
        x[which.max(x$numbers), ]
      }) %>% do.call(rbind, .)
    return(modes)
  }

summary_statistic_rank <-
  function(dependent.var, independent.var, design, confidence_level = 0.95) {
    percent <-
      percent_with_confints(dependent.var, independent.var, design, confidence_level = confidence_level)
    ranked <-
      percent %>% split.data.frame(percent$independent.var.value, drop = T) %>% lapply(function(x) {
        mutate(x, rank = rank(x$numbers, ties.method = "min"))
      }) %>% do.call(rbind, .)
    return(ranked)
  }

###function that takes a variable (vector of values) and checks if it has more than one unique values
var_more_than_n <- function(var, n) {
  var <- var[!is.na(var)]
  var <- trimws(var)
  if (length(unique(var[var != ""])) > n) {
    return(TRUE)
  }
  return(FALSE)
}

#### function that checks if a question is in the questionnaire
question_in_questionnaire <- function(var) {
  if (exists("questionnaire")) {
    result <- (sum(questionnaire$questions$name %in% var) > 0)
  } else{
    result <- FALSE
  }
  return(result)
}
