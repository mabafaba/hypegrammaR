# GENERIC LOW LEVEL SANITATIONS:

datasanitation_return_empty_table <- function(data, dependent.var, independent.var = NULL){

  #### this is really counter intuitive - why would an empty table return 1? -> especially in mean this should not happen.
  #### therefore added this row to go straight to NA.
  #### I'm assuming thought that this creats an issue elsewhere... if it's 100% of a single independent.var... then i guess it should be 1?
  #### not sure how to catch this _here_ though.. "datasanitation_return_empty_table" doesnt know if its numbers or cateogricals etc..
  #### I think maybe the return 1 should be a different function?
  #### not sure
  #### but changing this regardless.. function fails all the time, it's too convoluted further down; all the time the values submitted to the df to return have different lengths and breaks it.
  return(datasanitation_return_empty_table_NA(data,dependent.var,independent.var = independent.var))


  if(datasanitation_variables_in_data_colnames(data, dependent.var, independent.var)$success == F){
    return(datasanitation_return_empty_table_NA(data, dependent.var, independent.var))
  }
#
#   num_unique_dependent_values <- length(data[[dependent.var]] %>% unique %>% .[!is.na(.)])
#   num_unique_independent_values <- length(data[[independent.var]] %>% unique %>% .[!is.na(.)])
#   num_combos<-   num_unique_dependent_values* num_unique_independent_values
#
#
#   if(num_combos == 0){
#     return(datasanitation_return_empty_table_NA(dependent.var = dependent.var))
#   }
#
#   if(num_combos == 1){
#     numbers <- 1
#     dependent.var.value   <- data[[  dependent.var]] %>% unique %>% .[!is.na(.)]
#     independent.var.value <- data[[independent.var]] %>% unique %>% .[!is.na(.)]
#
#   }



  # expect a single category by default:
  numbers <- 1

  dependent.var.value <- unique(data[[dependent.var]])

  if(length(dependent.var.value[!is.na(dependent.var.value)]) == 1){
    numbers <- 1
  }else{
    numbers <- NA
  }



  if(!is.null(independent.var) & !is.na(independent.var) & length(independent.var) != 0 ){
    independent.var.value <- unique(data[[independent.var]])
    if(length(independent.var.value) < 1){
      independent.var.value<-NA
      numbers<-NA
    }

  }else{
    independent.var <- NA
    independent.var.value <- NA
    numbers <- NA
  }




  return(data.frame(
    dependent.var,
    independent.var = independent.var,
    dependent.var.value,
    independent.var.value = independent.var.value,
    numbers = numbers,
    se = NA,
    min = NA,
    max = NA
  )
  )
}

datasanitation_return_empty_table_NA <- function(data, dependent.var, independent.var = NULL){

return(data.frame(
  dependent.var,
  independent.var = NA,
  dependent.var.value = NA,
  independent.var.value = NA,
  numbers = NA,
  se = NA,
  min = NA,
  max = NA
)
)
}

datasanitation_is_good_dataframe<-function(data,...){
  if(!is.data.frame(data)){return(failed_sanitation("data is not a data frame"))}
  if(ncol(data)<1){return(failed_sanitation("data has no columns"))}
  if(nrow(data)<1){return(failed_sanitation("data has no rows"))}
  if(as.vector(data) %>% is.na %>% all){return(failed_sanitation("all data is NA"))}
  return(successfull_sanitation(data))
}

data_sanitation_remove_not_in_samplingframe<-function(data,samplingframe_object,name="samplingframe"){
  records_not_found_in_sf<-!(samplingframe_object$add_stratum_names_to_data(data)[,samplingframe_object$stratum_variable] %in% samplingframe_object$sampling.frame$stratum)
  if(length(which(records_not_found_in_sf))==0){
    .write_to_log(paste("samplingframe",name,"complete.\n"))
    return(data)
  }
  logfile<-paste0("./output/log/ERROR_LOG_records_discared",format(Sys.time(), "%y-%m-%d__%H-%M-%S"),".csv")
  write.csv(data[records_not_found_in_sf,],logfile)
  logmessage(paste("FATAL:",
                   length(which(records_not_found_in_sf)),
                   "records discarded, because they could not be matched with samplingframe.
                   I wrote a copy of those records to",logfile,
                   "\n sampling frame name:",name,"\n"))
  .write_to_log(paste("names not found in sampling frame:\n",
                      paste(samplingframe_object$add_stratum_names_to_data(data)[,samplingframe_object$stratum_variable] %>% unique,collapse="\n")
  ))

  return(data[!records_not_found_in_sf,])
}


### HOW TO RECODE BLANKS
# datasanitation_NA_heavy<-function(data,dependent.var,independent.var){
#   dependent_NA_heavy <- sum(is.null(data[[dependent.var]])) / length(data[[dependent.var]]) > 0.95
#   independent_NA_heavy <- length(is.na(data[[dependent.var]])) / length(data[[independent.var]]) > 0.95
#   if(!dependent_more_than_1){return(failed_sanitation("less than two unique values in the dependent variable"))}
#   return(successfull_sanitation(data))
# }

datasanitation_morethan_1_unique_dependent<-function(data,dependent.var,independent.var){
  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){return(failed_sanitation("less than two unique values in the dependent variable"))}
  return(successfull_sanitation(data))
}
datasanitation_morethan_1_unique_independent<-function(data,dependent.var,independent.var){
  independent_more_than_1 <- length(unique(data[[independent.var]])) > 1
  if(!independent_more_than_1){return(failed_sanitation("less than two unique values in the independent variable"))}
  return(successfull_sanitation(data))
}


datasanitation_remove_missing<-function(data,dependent.var,independent.var,...){
  data<-data[!is.na(data[[dependent.var]]),]
  data[[dependent.var]] <- as.character(data[[dependent.var]])
  data<-data[(data[[dependent.var]]!=""),]
  data<-data[(data[[dependent.var]]!="NA"),]
  data<-data[(data[[dependent.var]]!="<NA>"),]
  if(nrow(data)<=2){return(failed_sanitation("less than 3 records have valid values in the dependent variable and in the independent variable"))}
  return(successfull_sanitation(data))
}

datasanitation_variables_in_data_colnames<-function(data,dependent.var,independent.var,...){
  dep_var_name_in_data_headers<- grep(paste("^", dependent.var,"$", sep=""),colnames(data),value = T)
  if(!is.null(independent.var)){
  indep_var_name_in_data_headers<- grep(paste0("^", independent.var,"$"),colnames(data),value = T)}else{indep_var_name_in_data_headers <- T}
  if(length(dep_var_name_in_data_headers)==0){return(failed_sanitation(paste0("dependent variable \"",dependent.var,"\" not found in data.")))}
  if(length(indep_var_name_in_data_headers)==0){
    return(failed_sanitation(paste0("independent variable \"",independent.var,"\" not found in data.")))}
  return(successfull_sanitation(data))
}

datasanitation_independent_max_unique<-function(data,dependent.var,independent.var, n_max = 50){
  valid<-length(unique(data[[independent.var]])) <= n_max
  datasanitation_generic_check(data,dependent.var,independent.var,valid,paste0("too many (>=",n_max,") unique values in independent variable"))
}

datasanitation_dependent_max_unique<-function(data,dependent.var,independent.var, n_max = 30){
  valid<-length(unique(data[[dependent.var]])) <= n_max
  datasanitation_generic_check(data,dependent.var,independent.var,valid,paste0("too many (>=",n_max,") unique values in dependent variable"))
}

datasanitation_morethan_1_record_per_independent_value<-  function(data,dependent.var,independent.var){
  which_independent_more_than_one_record <- table(data[[independent.var]])
  which_independent_more_than_one_record <- which_independent_more_than_one_record[which(which_independent_more_than_one_record>1)]
  which_independent_more_than_one_record <- names(which_independent_more_than_one_record)
  data <- data[data[[independent.var]] %in% which_independent_more_than_one_record,]
  successfull_sanitation(data)
}

datasanitation_morethan_2_records_total<-function(data,dependent.var,independent.var,...){
  datasanitation_generic_check(data,dependent.var,independent.var,valid=nrow(data)>2,"less than 2 records two samples with valid data available for this combination of dependent and independent variable")
}


datasanitation_dependent_numeric<-function(data,dependent.var,independent.var,...){
  if(is.factor(data[[dependent.var]])){data[[dependent.var]]<-as.character(data[[dependent.var]])}
  data[[dependent.var]]<-suppressWarnings(as.numeric(data[[dependent.var]]))
  if(all(is.na(data[[dependent.var]]))){return(failed_sanitation("dependent variable is not numeric"))}
  data<-data[!is.na(data[[dependent.var]]),]
  return(successfull_sanitation(data))
}


datasanitation_independent_numeric<-function(data,dependent.var,independent.var,...){
  if(is.factor(data[[dependent.var]])){data[[dependent.var]]<-as.character(data[[dependent.var]])}
  data[[dependent.var]]<-as.numeric(data[[dependent.var]])
  if(all(is.na(data[[dependent.var]]))){return(failed_sanitation("independent variable is not numeric"))}
  data<-data[!is.na(data[[dependent.var]]),]
  return(successfull_sanitation(data))
}

as.numeric_factors_from_names<-function(x){
  if(is.factor((x))){x<-as.character(x)}
  return(as.numeric(x))
}

datasanitation_question_not_sm <- function(data,dependent.var,independent.var,...){
  if(!exists("questionnaire")) {
    dependent_is_select_multiple <- FALSE
  }else{dependent_is_select_multiple <- questionnaire$question_is_select_multiple(dependent.var)
    }
  if(dependent_is_select_multiple){return(failed_sanitation("Question is a select multiple. Please use percent_with_confints_select_multiple instead"))
  }
  return(successfull_sanitation(data))
}


datasanitation_question_sm <- function(data,dependent.var,independent.var,...){
  if(!exists("questionnaire")) {
    dependent_is_select_multiple <- TRUE
  }else{dependent_is_select_multiple <- questionnaire$question_is_select_multiple(dependent.var)
    }
  if(!dependent_is_select_multiple){return(failed_sanitation("Question is not select multiple, but the function expects one"))
  }
  return(successfull_sanitation(data))
}

datasanitation_dependent_select_one <- function(data,dependent.var,independent.var,...){
  if(!exists("questionnaire")) {
    dependent_is_select_one <- TRUE
  }else{dependent_is_select_one <- questionnaire$question_is_select_one(dependent.var)
  }
  if(!dependent_is_select_one){return(failed_sanitation("Dependent variable is not a select one (categorial), but the function expects one"))
  }
  return(successfull_sanitation(data))
}

datasanitation_independent_select_one <- function(data,dependent.var,independent.var,...){
  if(!exists("questionnaire")) {
    dependent_is_select_one <- TRUE
  }else{independent_is_select_one <- questionnaire$question_is_select_one(independent.var)
  }
  if(!independent_is_select_one){return(failed_sanitation("Independent variable is not a select one (categorial), but the function expects one"))
  }
  return(successfull_sanitation(data))
}

question_matches_choices <- function(data, dependent.var, sm.columns){
  if(!exists("questionnaire")) {return(NULL)
    }
  if(!questionnaire$question_is_select_multiple(dependent.var)){return(warning("Variable provided is not select multiple, Using only the choices to calculate summary statistics."))
    }
q_m_c <- all(questionnaire$choices_for_select_multiple(dependent.var, data) == sm.columns)
  if(!q_m_c){return(warning("The choices don't match the question provided. Using only the choices to calculate summary statistics."))
    }
}



