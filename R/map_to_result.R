#' Map to results from data, variable names & case
#'
#' Produce summary statistics, hypothesis tests and plot objects for a hypothesis
#'
#' @param data
#' @param dependent.var string with the column name in `data` of the dependent variable
#' @param independen.var string with the column name in `data` of the independent variable
#' @param case the analysis case, created with map_to_case().
#' @param cluster.variable.name if cluster sampling, provide the name of the variable in the dataset that denotes the cluster
#' @param weighting A function that generates weights from a dataframe. You can create it with surveyweights::weighting_fun_from_samplingframe()
#' @return A list with 1. the summary.statistic and 2. the hypothesis.test result
#' @examples
#' plot_crayons()
#' @export
map_to_result<-function(data,
                            dependent.var,
                            independent.var = NULL,
                            case,
                            cluster.variable.name=NULL,
                            weighting=function(df){rep(1,nrow(df))}){
  options(survey.lonely.psu = "average")


  # put the relevant input parameters in a list so we can attach them to the output:
  parameters<-list(
    dependent.var = dependent.var,
    independent.var = independent.var,
    cluster.variable.name=cluster.variable.name,
    weighted=is.function(weighting),
    case=case
  )

  # sanitise input

  # data <- data[!is.na(data[,dependent.var]),]
  # if(nrow(data)==0){stop('provided data has no rows where dependent.var is not NA')}
  # if(all(is.na(data[,dependent.var]))){stop(paste('variable', dependent.var, 'can\'t be all NA'))}
  if(!is.function(weighting)){
    stop("'weighting' must be a function. You can create one with weighting_fun_from_samplingframe(). You can also write your own function (must take a dataframe as input and return a scalar)")
  }


  # map from input to analysis case:
    if(!is_valid_case_string(case)){
      stop("value for argument 'case' is not a valid case string. It is best to create one using map_to_case(). If you make it by hand, it must be of the form CASE_[hypothesis_type]_[dependent.variable.type]_[independent.variable.type]\nfor example 'CASE_group_difference_categorical_categorical'\n")
    }



  data_sanitised<-sanitise_data(data,
                                dependent.var,
                                independent.var,
                                case)
  if(data_sanitised$success){
    data<-data_sanitised$data
  }else{
    return(
      list(parameters=parameters,
        summary.statistic=NULL,
        hypothesis.test.result=NULL,
        message=data_sanitised$message
      )

    )
  }


  # map from case to appropriate summary statistic, hypothesis test and visualisation:



  design <- map_to_design(data = data,
                          weighting_function = weighting,
                          cluster_variable_name = cluster.variable.name
  )

  summarise.result<- map_to_summary_statistic(case)
  test.hypothesis <- map_to_hypothesis_test(case)

  # apply the summary statistic, hypothesis test to the given data and survey design:
  summary.result  <- summarise.result(dependent.var,independent.var, design)
  # do hypothesis test:

  hypothesis.test.result<- test.hypothesis(dependent.var,independent.var, design)

  # add results to the visualisation:
  # visualisation<-visualisation+ggplot()...







  return(list(parameters=parameters,
              summary.statistic=summary.result,
              hypothesis.test=hypothesis.test.result,
              message="success (or unidentified issue)"
  ))

}

