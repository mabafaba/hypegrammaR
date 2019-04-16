

list_all_cases<-function(implemented_only=F){

  if(!implemented_only){
    hypothesis_types<-c("direct_reporting","group_difference","limit","correlation","change")
    dependent.var.types<-c("numerical","categorical")
    independent.var.types<-c("numerical","categorical")
    valid_cases<- apply(expand.grid("CASE",hypothesis_types, dependent.var.types,independent.var.types), 1, paste, collapse="_")
    return(valid_cases)
  }

  return(c(
    "CASE_group_difference_categorical_categorical",
    "CASE_group_difference_numerical_categorical",
    "CASE_direct_reporting_numerical_",
    "CASE_direct_reporting_categorical_",
    "CASE_direct_reporting_categorical_categorical",
    "CASE_direct_reporting_numerical_categorical",
    "CASE_limit_categorical",
    "CASE_limit_numerical",
    "CASE_correlation_numerical_numerical",
    "CASE_correlation_categorical_numerical"
  ))
}


is_valid_case_string<-function(x,implemented_only=T){

  return(x %in% list_all_cases(implemented_only = implemented_only))

}

case_not_implemented_error<-function(case,situation){
  stop(paste(situation,":  case", case, "not implemented"))
}



#' Map to case
#'
#' creates a string that other functions can use to know what analysis case they are dealing with
#'
#' @param hypothesis.type The hypothesis type. Must be one of "group_difference" or "direct_reporting".
#' @param dependent.var.type The type of the dependent variable as a string. must be either "numerical" or "categorical"
#' @param independent.var.type The type of the independent variable as a string. must be either "numerical" or "categorical"
#' @return a string that other functions can use to know what analysis case they are dealing with. It has a class "analysis_case" assigned
#' @examples map_to_case()
#' @export
map_to_case<-function(hypothesis.type,
                      dependent.var.type=NULL,
                      independent.var.type=NULL
){
  if(!is.null(independent.var.type)){
    if(!(independent.var.type %in% c("numerical","categorical"))){
      stop("dependent.var.type must be either 'categorical', 'numerical' or left empty (guessing from data)")
    }
  }

  if(!(dependent.var.type %in% c("numerical","categorical"))){
    stop(  "dependent.var.type must be either 'categorical', 'numerical' or left empty (guessing from data)")
  }

  if(hypothesis.type!="direct_reporting" & is.null(independent.var.type)){
    stop("if hypothesis type is not 'direct_reporting, the independent.var.type must be provided (and not be NULL)'")
  }

  paired=NULL
  variable.type <- paste0(dependent.var.type, "_",
                          independent.var.type)
  case <- paste(c("CASE",hypothesis.type,variable.type, paired), collapse = "_")
  class(case)<-"analysis_case"
  return(case)
}








#'
#' #' Guess the  case
#' #'
#' #' creates a string that other functions can use to know what analysis case they are dealing with
#' #'
#' #' @param data missing documentation
#' #' @param hypothesis.type
#' #' @param dependent.var
#' #' @param independent.var
#' #' @param paired
#' #' @return a string that other functions can use to know what analysis case they are dealing with. It has a class "analysis_case" assigned
#' #' @examples map_to_design(data,cluster_variable_name="cluster_id")
#' #' @export
#' guess_to_case<-function(data,
#'                         hypothesis.type,
#'                         dependent.var,
#'                         independent.var = NULL,
#'                         dependent.var.type=NULL,
#'                         independent.var.type=NULL,
#'                         paired = NULL){
#'
#'   if(!is.null(independent.var.type)){
#'     if(!(independent.var.type %in% c("numerical","categorical"))){
#'       stop("dependent.var.type must be either 'categorical', 'numerical' or left empty (guessing from data)")
#'     }
#'   }
#'
#'   if(!is.null(dependent.var.type)){
#'     if(!(dependent.var.type %in% c("numerical","categorical"))){
#'       stop(  "dependent.var.type must be either 'categorical', 'numerical' or left empty (guessing from data)")
#'     }
#'   }
#'
#'   guess_type<-function(x){
#'     if(length(which(is.na(x)))==length(which(is.na(as.numeric(x))))){
#'       return("numerical")
#'     }else{
#'       return("categorical")
#'     }
#'   }
#'   if(is.null(dependent.var.type)){dependent.var.type<-guess_type(data[[dependent.var]])}
#'   if(is.null(independent.var.type)){independent.var.type<-guess_type(data[[independent.var]])}
#'   variable.type <- paste0(dependent.var.type, "_",
#'                           independent.var.type)
#'   case <- paste(c("CASE",hypothesis.type,variable.type, paired), collapse = "_")
#'   class(case)<-"analysis_case"
#'   return(case)
#' }



