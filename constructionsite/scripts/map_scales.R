#################################
# map to case:                  #
#################################
map_to_case<-function(data,
                        hypothesis.type,
                        dependent.var,
                        independent.var,
                        paired = NULL){
  
  
  variable.type <- paste0(reachR:::variable_type(dependent.var), "_", reachR:::variable_type(independent.var))
  case <- paste(c("CASE",hypothesis.type,variable.type, paired), collapse = "_")
  class(case)<-"analysis_case"
  return(case)
}


list_all_cases<-function(implemented_only=F){
  
  if(!implemented_only){
  hypothesis_types<-c("direct_reporting","group_difference","limit","correlation","change")
  dependent.var.types<-c("numerical","categorical")
  independent.var.types<-c("numerical","categorical")
  valid_cases<- apply(expand.grid("CASE",hypothesis_types, dependent.var.types,independent.var.types), 1, paste, collapse="_")  
  return(valid_cases)
  }
  
  return(c(
    "CASE_group_difference_categorical_categorical"
  ))
}


is_valid_case_string<-function(x,implemented_only=T){
  
  return(x %in% list_all_cases(implemented_only = implemented_only))
  
}

case_not_implemented_error<-function(case,situation){
  stop(paste(situation,":  case", case, "not implemented"))
}



#################################
# map to summary statistic:     #
#################################


map_to_summary_statistic <- function(case) {
    # sanitise input:
    if(!is_valid_case_string(case)){stop("input to map_to_summary_statistic must be a valid analysis case")}

  # define summary functions for all cases:
  summary_functions<-list()
  summary_functions$CASE_group_difference_categorical_categorical <- percent_with_confints
  # summary_functions$CASE_group_difference_numerical_numerical <- function(...){stop(paste("summary statistic for case",case,"not implemented"))}hypothesis_test_one_sample_t
  # summary_functions$CASE_group_difference_numerical_numerical <- hypothesis_test_one_sample_t
  
  
  
  # return corresponding summary function:
  
  return(summary_functions[[case]])

}

#################################
# map to hypothesis test  :     #
#################################


map_to_hypothesis_test <- function(case) {
  hypothesis_test_functions<-list()
  # prefill all valid cases with 'not implemented' errors:
  lapply(list_all_cases(implemented_only = F),function(x){
    hypothesis_test_functions[[x]]<-case_not_implemented_error(case,"hypothesis test")
  })
  
  # add implemented cases:
  hypothesis_test_function[["CASE_group_difference_categorical_categorical"]] <- hypothesis_test_chisquared

  return(hypothesis_test_functions[[case]]) 
  }



#################################
# map to visualisation:         #
#################################
map_to_visualisation <- function(case) {
  visualisation_functions<-list()
  # prefill all valid cases with 'not implemented' errors:
  lapply(list_all_cases(implemented_only = F),function(x){
    hypothesis_test_functions[[x]]<-case_not_implemented_error(case,"hypothesis test")
  })
  
  # add implemented cases:
  visualisation_functions[["CASE_group_difference_categorical_categorical"]] <- hypothesis_test_chisquared
  
  return(hypothesis_test_functions[[case]]) 
}



