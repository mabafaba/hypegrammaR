#' Map to results from data, variable names & case
#'
#' Produce summary statistics, hypothesis tests and plot objects for a hypothesis
#'
#' @param data the data as a data.frame. Must match the sampling frame used to produce the `weighting` as well as the questionnaire if applicable.
#' @param dependent.var string with the column name in "data" of the dependent variable
#' @param independen.var string with the column name in `data` of the independent variable
#' @param case the analysis case, created with map_to_case().
#' @param cluster.variable.name if cluster sampling, provide the name of the variable in the dataset that denotes the cluster
#' @param weighting A function that generates weights from a dataframe. You can create it with surveyweights::weighting_fun_from_samplingframe()
#' @param questionnaire output from load_questionnaire()
#' @details
#'
#' - takes as parameters outputs from
#'     - load_data()
#'     - map_to_case()
#'     - load_samplingframe() %>% map_to_weighting()
#'     - load_questionnaire()
#' - output can be processed by:
#'     - map_to_labeled()
#'     - map_to_visualisation()
#'     - map_to_table()
#'     - map_to_master_table()
#'     - map_to_visualisation_heatmap()
#'
#' @return A list with the summary.statistic the hypothesis.test result
#' @export
map_to_result<-function(data,
                        dependent.var,
                        independent.var = NULL,
                        case,
                        cluster.variable.name=NULL,
                        weighting=function(df){rep(1,nrow(df))},
                        questionnaire=NULL){

  options(survey.lonely.psu = "remove")

  # put the relevant input parameters in a list so we can attach them to the output:
  parameters<-list(
    dependent.var = dependent.var,
    independent.var = independent.var,
    cluster.variable.name=cluster.variable.name,
    weighted=is.function(weighting),
    case=case
  )


  if(is.null(weighting)){
    weighting=function(df){rep(1,nrow(df))}
  }
  if(!is.function(weighting)){
    stop("'weighting' must be a function. You can create one with weighting_fun_from_samplingframe(). You can also write your own function (must take a dataframe as input and return a scalar)")
  }


  # map from input to analysis case:
    if(!is_valid_case_string(case)){
      stop("value for argument 'case' is not a valid case string. It is best to create one using map_to_case(). If you make it by hand, it must be of the form CASE_[hypothesis_type]_[dependent.variable.type]_[independent.variable.type]\nfor example 'CASE_group_difference_categorical_categorical'\n")
    }




  design <- map_to_design(data = data,
                          weighting_function = weighting,
                          cluster_variable_name = cluster.variable.name)



  hypothesis.test.result<-  map_to_hypothesis_test(design = design,
                                            dependent.var = dependent.var,
                                            independent.var = independent.var,
                                            case = case,
                                            questionnaire = questionnaire)

  summary.result  <- map_to_summary_statistic(design = design,
                                              dependent.var = dependent.var,
                                              independent.var = independent.var,
                                              case = case,
                                              questionnaire = questionnaire)






  return(list(parameters=parameters,
              summary.statistic=summary.result,
              hypothesis.test=hypothesis.test.result,
              message="success (or unidentified issue)"
  ))

}

