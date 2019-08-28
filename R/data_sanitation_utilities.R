#' Applies basic sanitation to data before summary statistics or hypothesis test can be applied
#'
#' @param design the design object
#' @param dependent.var a string containing the dependent variable in the analysis case
#' @param independent.var a string containing the independent variable in the analysis case
#' @param sanitation_function the function containing all the checks for the analysis function in question
#' @return returns the cleaned data with a santation success or failure message
#' @export
datasanitation_design<-function(design,dependent.var,independent.var,sanitation_function){
  sanitised<-sanitation_function(design$variables,dependent.var,independent.var)
  if(sanitised$success){
    sanitised$design<-map_to_design(sanitised$data)
  }else{
    sanitised$design<-NULL
  }
  return(sanitised)
}


# BLOCK SPECIFIC SANITATIONS:

datasanitation_summary_statistics_percent_with_confints_select_one <- function(data,dependent.var,independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_question_not_sm,
                         datasanitation_morethan_1_unique_dependent)
}


datasanitation_summary_statistics_percent_sm_choice <- function(data,dependent.var,independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_dependent_max_unique)
}

datasanitation_summary_statistics_percent_sm_choice_groups <- function(data,dependent.var,independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_dependent_max_unique,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_independent_max_unique)}


datasanitation_summary_statistics_percent_groups <- function(data,dependent.var,independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_question_not_sm,
                         datasanitation_dependent_max_unique,
                         datasanitation_independent_max_unique)
}

datasanitation_summary_statistics_mean <- function(data, dependent.var, independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_dependent_numeric)
}

datasanitation_summary_statistics_mean_groups <- function(data, dependent.var, independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_dependent_numeric,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_independent_max_unique)
}




datasanitation_hypothesistest_chisq<-function(data,dependent.var,independent.var){
  # apply an exquisite selection of sanitations functions relevant to chisquare hypothesis tests:


  apply_data_sanitations(data,           # all functions take these parameters
                         dependent.var,  # all functions take these parameters
                         independent.var,# all functions take these parameters
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_independent_max_unique,
                         datasanitation_dependent_max_unique,
                         datasanitation_morethan_1_record_per_independent_value

  )

  }


datasanitation_hypothesistest_chisq_sm<-function(data,dependent.var,independent.var){
  # apply an exquisite selection of sanitations functions relevant to chisquare hypothesis tests:


  apply_data_sanitations(data,           # all functions take these parameters
                         dependent.var,  # all functions take these parameters
                         independent.var,# all functions take these parameters
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_independent_max_unique,
                         datasanitation_dependent_max_unique,
                         datasanitation_morethan_1_record_per_independent_value

  )

}

datasanitation_hypothesistest_t<-function(data,dependent.var,independent.var){
  # apply an exquisite selection of sanitations functions relevant to chisquare hypothesis tests:


  apply_data_sanitations(data,           # all functions take these parameters
                         dependent.var,  # all functions take these parameters
                         independent.var,# all functions take these parameters
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_dependent_numeric,
                         datasanitation_independent_max_unique,
                         datasanitation_morethan_1_record_per_independent_value

  )
}


datasanitation_hypothesistest_limit<-function(data,dependent.var,independent.var){
  # apply an exquisite selection of sanitations functions relevant to chisquare hypothesis tests:


  apply_data_sanitations(data,           # all functions take these parameters
                         dependent.var,  # all functions take these parameters
                         independent.var,# all functions take these parameters
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_dependent_numeric
                         )
}

datasanitation_logistic_regression <- function(data, dependent.var, independent.var){
  apply_data_sanitations(data,
                         dependent.var,
                         independent.var,
                         datasanitation_morethan_1_unique_dependent,
                         datasanitation_morethan_1_unique_independent,
                         datasanitation_dependent_numeric,
                         datasanitation_independent_numeric
  )
}


# GENERIC SANITATION GROUPS:
datasanitation_always_applicable_before<-function(data,dependent.var,independent.var,...){
  apply_data_sanitations(data,dependent.var,independent.var,
                         datasanitation_is_good_dataframe,
                         datasanitation_variables_in_data_colnames,
                         datasanitation_remove_missing,
                         BEFORE=NULL, # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION (scary innit)
                         AFTER=NULL   # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION
  )
}

datasanitation_always_applicable_after<-function(data,dependent.var,independent.var,...){
  apply_data_sanitations(data,dependent.var,independent.var,
                         datasanitation_is_good_dataframe,
                         datasanitation_morethan_2_records_total,
                         BEFORE=NULL, # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION
                         AFTER=NULL   # apply_data_sanitations() applies this function; overwritting it to prevent INFINITE RECURSION
  )
}


# STANDARD FORMATS
# centralising the output format (insuring it's standardised and preventing repetition):
failed_sanitation<-function(message){
  return(list(data=NULL,message=message,success=F))
}

successfull_sanitation<-function(data){
  return(list(data=data,message=NA,success=T))
}

datasanitation_generic_check<-function(data,dependent.var,independent.var,valid,message=""){
  if(valid){return(successfull_sanitation(data))}else{return(failed_sanitation(message))}
}

# CHAINING SANITATION FUNCTIONS:
apply_data_sanitations<-function(data,dependent.var,independent.var,...){
  # ... should be sanitation functions.
  # call like this:
  # apply_data_sanitations(data,"myvarname","myothervarname",
  #                        datasanitation_remove_missing_data,
  #                        datasanitation_dependent_morethan_1_unique,
  #                        datasanitation_independent_morethan_1_unique)
  # get the "..." parameters as a list; add generic tests (can be overwritten by passing datasanitation_always_applicable_before/after as parameters!)
  params<-list(...)
  # allow overwriting 'before' and 'after' generic tests by passing 'BEFORE' and 'AFTER' named arguments:
  if("BEFORE" %in% names(list(...))){before<-params$BEFORE}else{before<-datasanitation_always_applicable_before}
  if("AFTER" %in% names(list(...))){after<-params$AFTER}else{after<-datasanitation_always_applicable_after}

  sanitation_functions<-list(before,...,after)



  # this has to be sequential, so here's a loop

  data_sanitised<-successfull_sanitation(data) # starting
  # for each sanitation function..

  for(i in c(1:length(sanitation_functions))){
    # take the i'th function

    currentfun<-sanitation_functions[[i]]
    if(is.null(currentfun)){next}
    # apply the function
    data_sanitised<-currentfun(data,dependent.var,independent.var)
    # if sanitation failed, quit sanitation (return), and return an empty sanitation with the message:
    if(data_sanitised$success==F){return(data_sanitised)}
    # otherwise, go ahead with the next sanitation
    data <- data_sanitised$data # after updating the data!!!
  }
  return(data_sanitised)
}




