
#' these functions should be depreciated eventually, but for now still in use in hypothesis_tests.R
#' they should go because now there is a proper system to apply data sanitation within each summary statistic and hypothesis test function.



#' take the data and some information about the planned analysis and prepare the data to ensure analysis doesn't fail
#' @param data data
#' @param dependent.var the name of the dependent variable
#' @param independent.var the name of the independent variable
#' @param case the analysis case (see map_to_case())
#' @return a list with two items:
#' - 1. `data`:  the sanitised dataset
#' - 2. `success` a logical value, TRUE if the data can be analysed,FALSE if the sanitation was unsuccessful
#'
#' if `$success` is FALSE, there may be a `$message` with more information, and the data will be NULL.
#'
sanitise_data <- function(data, dependent.var,independent.var,case){

  # data<-tryCatch({
  #   numerics<-sapply(names(data),question_is_numeric)
  #   data[,numerics]<-data[,numerics] %>% lapply(as.numeric) %>% as.data.frame(stringsAsFactors=F)
  #   data
  #   },
  #   error=function(e){return(data)}
  # )
  #

  if(is.null(independent.var)){
    sanitise_data_no_independent(data = data, dependent.var = dependent.var, case = case)}else{
      sanitise_data_independent(data = data, independent.var = independent.var, dependent.var = dependent.var, case = case)
    }
}

sanitise_data_no_independent<-function(data,
                                       dependent.var,
                                       case){

  dep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  indep_var_name_in_data_headers<- grep(paste0("^",dependent.var),colnames(data),value = T)
  if(length(dep_var_name_in_data_headers)==0){
    stop(paste0("dependent.var: \"",dependent.var,"\" not found in data"))
  }



  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="not a data frame or data frame without data"))}

  # remove records with NA in dependent or independent
  data<-data[!is.na(data[[dependent.var]]),]
  data<-data[(data[[dependent.var]]!=""),]


  # still have enough data?

  dependent_more_than_1 <- length(unique(data[[dependent.var]])) > 1
  if(!dependent_more_than_1){
    return(list(success=FALSE,message="can not summarise statistics with <2 different values in the dependent variable"))
  }


  if(!sanitise_is_good_dataframe(data)){return(list(success=F,message="no data (after removing records with NA in dependent variable)"))}
  return(list(success=T,data=data))
}

sanitise_data_independent<-function(data,
                                    dependent.var,
                                    independent.var,
                                    case){

#
  if((grep("group_difference",case) %>% length)>0){
#     group_difference<-sanitise_group_difference(data,
#                                                 dependent.var = dependent.var,
#                                                 independent.var = independent.var)
#     if(group_difference$success==F){return(group_difference)}
    return(list(success=T,data=data))
  }

  if(case%in%c("CASE_group_difference_categorical_categorical","CASE_direct_reporting_categorical_categorical","CASE_direct_reporting_categorical_")){
      dependent.var_num_unique<-data[[dependent.var]] %>% as.character %>% strsplit(" ") %>% unlist %>% unique %>% length
      if(!coercible_to_numeric(data[[dependent.var]]) & dependent.var_num_unique>50){
      return(list(success=F,message="can not perform chisquared test (and won't calculate summary statistics) on >50 unique values in the dependent variable."))

    }
  }
  if(!(nrow(data)>=2)){return(list(success=F,message="less than two samples with valid data available for this combination of dependent and independent variable"))}
  return(list(success=T,data=data))


}













