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
      return(datasanitation_return_empty_table(data = design$variables, dependent.var, independent.var,message = sanitised$message))}


    design<-sanitised$design

    if(length(unique(design$variables[[dependent.var]]))==1 & length(levels(design$variables[[dependent.var]]))<=1){


      all_1_table<-data.frame(dependent.var = dependent.var,
                 independent.var = NA,
                 independent.var.value = NA,
                 dependent.var.value = unique(design$variables[[dependent.var]]),
                 numbers = 1,
                 se = NA, min = NA, max = NA)
      attributes(all_1_table)$hg_summary_statistic_fail_message <- "only one unique value in the dependent variable"


      return(
        all_1_table
      )
    }


    tryCatch(
      expr = {


        srvyr_design <- srvyr::as_survey_design(design)
        srvyr_design_grouped <- srvyr::group_by_(srvyr_design,dependent.var)
        result <- srvyr::summarise(srvyr_design_grouped,
                                   numbers = srvyr::survey_mean(vartype = "ci",
                                                                level = confidence_level))

        get_confints<-purrr::possibly(function(...){

          confints<-survey::svymean(x = formula(paste0('~',dependent.var)),
                                    design = srvyr_design) %>% confint(level = confidence_level)

        },otherwise = matrix(NA,ncol = 2,nrow = nrow(result)))

        confints<-get_confints()

        result$numbers_low<-confints[,1]
        result$numbers_upp<-confints[,2]



        result_hg_format <- data.frame(dependent.var = dependent.var,
                                       independent.var = NA, dependent.var.value = result[[dependent.var]],
                                       independent.var.value = NA, numbers = result$numbers,
                                       se = NA, min = result$numbers_low, max = result$numbers_upp)

        return(result_hg_format)},
      error = function(e) {
        .write_to_log("percent_with_confints_select_one failed with error:")
        .write_to_log(e$message)
        return(datasanitation_return_empty_table(dependent.var = dependent.var,independent.var = independent.var,message = e$message))
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
  for(x in dependent.var.sm.cols){
    dependent.var.check <- names(design$variables)[x]
    sanitised<-datasanitation_design(design,dependent.var.check,independent.var = NULL,
                                     datasanitation_summary_statistics_percent_sm_choice)
    if(!sanitised$success){
      warning(sanitised$message)
      return(datasanitation_return_empty_table(data = design$variables, dependent.var.check, message =sanitised$message))
      }
    design<-sanitised$design
    }

  ###

  # Get the columns with the choices data into an object
  choices <- design$variables[, dependent.var.sm.cols]

              results_srvyr <- lapply(names(choices), function(x) {
                design$variables[[x]] <- factor(as.logical(as.numeric(design$variables[[x]])),
                                                levels = c("TRUE", "FALSE"))
                srvyr_design <- srvyr::as_survey_design(design)
                srvyr_design_grouped <- srvyr::group_by_(srvyr_design,
                                                         x)
                result <- srvyr::summarise(srvyr_design_grouped, numbers = srvyr::survey_mean(vartype = "ci",
                                                                                              level = confidence_level))
              })

              results_srvyr <- results_srvyr %>% purrr::map(function(x){
                if(nrow(x)==0){
                  x[1,]<-c(NA,NA,NA,NA)
                  return(x)
                }
                x$dependent.var.value<-gsub(
                  paste0('^',dependent.var,"\\."),
                  "",
                  names(x)[1])

                x<-x[x[,1]=="TRUE"|is.na(x[,1]),]
                # names(x)[1]<-"numbers"

                x[,-1]
                # names(x)[1]<-names(choices)
              })



  results_srvyr %<>% do.call(rbind, .)
  # standard columns:
  results_srvyr %<>% rename('min' = 'numbers_low','max' = 'numbers_upp')
  results_srvyr$dependent.var <- dependent.var
  results_srvyr$independent.var <-NA
  results_srvyr$independent.var.value <-NA
  results_srvyr$se <-NA
  results <- results_srvyr %>% select(dependent.var,independent.var,dependent.var.value,independent.var.value,numbers,se,min,max)

  # trunkate confints to 0-1:
  results[, "min"] <-
    results[, "min"] %>% replace(results[, "min"] < 0 , 0)
  results[, "max"] <-
    results[, "max"] %>% replace(results[, "max"] > 1 , 1)
  # results %>% as.data.frame(stringsAsFactors = FALSE)

  return(results)
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


  # design$variables %>% split.data.frame(design$variables['independent.var']) %>%
  #   lapply(split.data.frame(design$variables['dependent.var'])){
  #
  #   }


  if(!sanitised$success){
    warning(sanitised$message)
    return(datasanitation_return_empty_table(data = design$variables, dependent.var,independent.var, message = sanitised$message))}

    design<-sanitised$design

    design$variables[[dependent.var]] <-
      as.factor(design$variables[[dependent.var]])

    design$variables[[independent.var]] <-
      as.factor(design$variables[[independent.var]])



    # if independent.var has only one level, redirect to percent_with_confints
      # if(!is.factor(design$variables[[independent.var]])){
      independent_levels<-unique(design$variables[[independent.var]])

      # }else{
      # independent_levels<-levels(design$variables[[independent.var]])
      # }
      if(length(independent_levels)<=1){
        sumstat <- percent_with_confints_select_one(dependent.var,design = design,
                                                    confidence_level = confidence_level)
        sumstat$independent.var<-independent.var

        sumstat$independent.var.value <- independent_levels
        return(sumstat[,c('dependent.var','independent.var','dependent.var.value','independent.var.value', 'numbers','se','min','max')])
      }


      if(length(unique(design$variables[[dependent.var]]))==1 & length(levels(design$variables[[dependent.var]]))<=1){


        result_counts <- table(design$variables[[dependent.var]],design$variables[[independent.var]]) %>% as.data.frame
        colnames(result_counts)<-c("dependent.var.value","independent.var.value","n")
        result_counts$nums<-rep(1,nrow(result_counts))
        result_counts$nums[result_counts$n==0]<-NA

        return(data.frame(dependent.var = dependent.var,
                          independent.var = independent.var,
                          independent.var.value = result_counts$independent.var.value,
                          dependent.var.value = result_counts$dependent.var.value,
                          numbers = result_counts$nums,
                          se = NA, min = NA, max = NA)

        )
      }





      srvyr_design <- srvyr::as_survey_design(design)

      srvyr_design_grouped <- group_by_(srvyr_design,independent.var, dependent.var)

      result <- summarise(srvyr_design_grouped,numbers = srvyr::survey_mean(vartype = "ci",
                                                                  level = confidence_level)
      )


    result_hg_format <-  data.frame(
          dependent.var = dependent.var,
          independent.var = independent.var,
          dependent.var.value = result[[dependent.var]],
          independent.var.value = result[[independent.var]],
          numbers = result$numbers,
          se = NA,
          min = result$numbers_low,
          max = result$numbers_upp
        )



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

    question_matches_choices(design$variables, dependent.var, sm.columns = dependent.var.sm.cols)


    ### Sanitation checks
    for(x in dependent.var.sm.cols){

      dependent.var.check <- names(design$variables)[x]
      sanitised<-datasanitation_design(design,dependent.var.check,independent.var = NULL,
                                       datasanitation_summary_statistics_percent_sm_choice)
      if(!sanitised$success){
        warning(sanitised$message)
        return(datasanitation_return_empty_table(data = design$variables, dependent.var.check, message = sanitised$message))
      }
      design<-sanitised$design
    }

    ###
    # if independent.var has only one level, redirect to percent_with_confints_select_multiple (no groups)
    # if(!is.factor(design$variables[[independent.var]])){
    independent_levels<-unique(design$variables[[independent.var]])
    # }else{
    # independent_levels<-levels(design$variables[[independent.var]])
    # }

    if(length(independent_levels)<=1){

      sumstat <- percent_with_confints_select_multiple(
        dependent.var,
        dependent.var.sm.cols = dependent.var.sm.cols,
        design = design,
        confidence_level = confidence_level)

      sumstat$independent.var<-independent.var

      sumstat$independent.var.value <- independent_levels
      return(sumstat[,c('dependent.var','independent.var','dependent.var.value','independent.var.value', 'numbers','se','min','max')])

    }



    # Get the columns with the choices data into an object
    choices <- design$variables[, dependent.var.sm.cols]


    result_hg_format <- lapply(names(choices), function(x) {
      design$variables[[x]] <- factor(as.logical(as.numeric(design$variables[[x]])),
                                      levels = c("TRUE", "FALSE"))
      srvyr_design <- srvyr::as_survey_design(design)
      srvyr_design_grouped <- srvyr::group_by_(srvyr_design,
                                               independent.var, x)
      result <- srvyr::summarise(srvyr_design_grouped, numbers = srvyr::survey_mean(vartype = "ci",
                                                                                    level = confidence_level))

      result<-result[result[,2]=="TRUE",]

      if (nrow(result) > 0) {
        summary_with_confints <- data.frame(dependent.var = dependent.var,
                                            independent.var = independent.var,
                                            dependent.var.value = gsub(paste0("^", dependent.var, "."), "", x),
                                            independent.var.value = result[,1],
                                            numbers = result$numbers, se = NA, min = result$numbers_low,
                                            max = result$numbers_upp)
      }
      else {
        summary_with_confints <- data.frame(dependent.var = dependent.var,
                                            independent.var = NA, dependent.var.value = gsub(paste0("^",

                                                                                                    dependent.var, "."), "", x), independent.var.value = NA,
                                            numbers = NA, se = NA, min = NA, max = NA)
      }
    })
    result_hg_format %<>% do.call(rbind, .)
    result_hg_format[, "min"] <- result_hg_format[, "min"] %>%
      replace(result_hg_format[, "min"] < 0, 0)
    result_hg_format[, "max"] <- result_hg_format[, "max"] %>%
      replace(result_hg_format[, "max"] > 1, 1)

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
    return(datasanitation_return_empty_table(design$variables, dependent.var, message = sanitised$message))}

  design<-sanitised$design


  formula_string <- paste0("~as.numeric(", dependent.var, ")")
  summary <- svymean(formula(formula_string),
                     design,
                     na.rm = T)

  confints <- confint(summary, level = confidence_level)


  if(!dependent.var=="dependent.var"){
    design$variables$dependent.var <-design$variables[[dependent.var]]
  }

  # srvyr_design <- srvyr::as_survey_design(design)
  #
  #
  # result <- srvyr::summarise(srvyr_design,numbers = srvyr::survey_mean(dependent.var,vartype = "ci",
  #                                                                              level = confidence_level)
  # )



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
    return(datasanitation_return_empty_table_NA(design$variables, dependent.var, independent.var, message = sanitised$message))
  }

  design<-sanitised$design

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

