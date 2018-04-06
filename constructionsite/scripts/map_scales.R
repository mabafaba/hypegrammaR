choose.test <- function(hypothesis.type, variable.types, crit = NULL, paired = NULL){
  
  typestring <- paste(c("TYPE",hypothesis.type,variable.types, paired), collapse = "_")
  # typestring="TYPE_direct_reporting_simple_random_numeric"
  TYPE_group_difference_categorical_categorical <- hypothesis_test_chisquare
  # TYPE_group_difference_numeric_categorical <- hypothesis_test_difference_in_means
  # TYPE_limit_numeric <- hypothesis_test_one_sample_z_num
  # TYPE_limit_categorical <- hypothesis_test_one_sample_z_cat
  TYPE_direct_reporting_numeric <- confidence_intervals_num
  # TYPE_direct_reporting_categorical <- confidence_intervals_cat
  # TYPE_change_over_time_numeric <- hypothesis_test_difference_in_means #paired or unpaires
  # TYPE_change_over_time_categorical <- hypothesis_test_chisquare
  # TYPE_correlation_numeric_numeric <- hypothesis_test_regression
  # TYPE_correlation_categorical_numeric <- hypothesis_test_logistic_regression #warn: categorical variable must be binary (ensure)
  #
  
  return(get(typestring))
  
}

#harmonise parameter names for named data columns:   [SOURCE].[WHAT].column: e.g. data.dependent.var.column, samplingframe.stratum.column, etc.

# I think this function should just take any number of names (either as a vector or with ...). Then get the type for each and concat them together.
# considering that some are dependent and some are independent is not a thing "find.data.types" should have to worry about.
# it should find data types for whatever you give to it and that's it.

find.data.types <- function(data.dependent.var, independent.var = NULL) {
  

  
  data.type.dep = c()
  data.type.indep = c()

  if(question_is_categorical(data.dependent.var) == T){
    data.type.dep = "categorical"
  }
  if(question_is_numeric(data.dependent.var) == T){
    data.type.dep = "numeric"
  }
  if(question_is_categorical(independent.var) == T){
    data.type.indep = "categorical"
  }
  if(question_is_numeric(independent.var) == T){
    data.type.indep = "numeric"
  }
  # fixed the underscore etc. issue
  variable.types <- paste(c(data.type.dep, data.type.indep), collapse="_")
  return(variable.types)
}


choose.stat<-function(x){
  
}


