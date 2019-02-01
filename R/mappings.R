#' Map to Design
#'
#' creates a `survey` design object from the data
#'
#' @param data
#' @param weighting_function if cluster sampling was used, what's the name of the column in `data` that identifies the cluster?
#' @details create a `survey` package design object from the data and information on the sampling strategy
#' @return a `survey` package design object
#' @examples map_to_design(data,cluster_variable_name="cluster_id")
#' @export
map_to_design <- function(data,
                          cluster_variable_name = NULL,
                          weighting_function = NULL) {

  # if no weighting function / cluster variable provided, set defaults, otherwise use parameters:
  if(is.null(cluster_variable_name)){
    cluster.ids <- as.formula(c("~1"))}else{
    cluster.ids <- paste0("~",cluster_variable_name)}
  if(is.null(weighting_function)){
    strata.weights<-rep(1,nrow(data))
  }else{
    strata.weights <- weighting_function(data)
  }

  survey.design <- svydesign(data = data,
      ids = formula(cluster.ids),
      strata = names(strata.weights),
      weights = as.vector(strata.weights),nest = T)
    return(survey.design)}
#add to this an option that strata weights can be the vector of weights if there is one in the data & warning that we usually dont do this

#' Map to case
#'
#' creates a string that other functions can use to know what analysis case they are dealing with
#'
#' @param data missing documentation
#' @param hypothesis.type
#' @param dependent.var
#' @param independent.var
#' @param paired
#' @return a string that other functions can use to know what analysis case they are dealing with. It has a class "analysis_case" assigned
#' @examples map_to_design(data,cluster_variable_name="cluster_id")
#' @export
map_to_case<-function(data,
                      hypothesis.type,
                      dependent.var,
                      independent.var = NULL,
                      dependent.var.type=NULL,
                      independent.var.type=NULL,
                      paired = NULL){

  if(!is.null(independent.var.type)){
    if(!(independent.var.type %in% c("numerical","categorical"))){
      stop("dependent.var.type must be either 'categorical', 'numerical' or left empty (guessing from data)")
    }
  }

  if(!is.null(dependent.var.type)){
    if(!(dependent.var.type %in% c("numerical","categorical"))){
      stop(  "dependent.var.type must be either 'categorical', 'numerical' or left empty (guessing from data)")
    }
  }

  guess_type<-function(x){
    if(length(which(is.na(x)))==length(which(is.na(as.numeric(x))))){
      return("numerical")
    }else{
      return("categorical")
    }
  }
  if(is.null(dependent.var.type)){dependent.var.type<-guess_type(data[[dependent.var]])}
  if(is.null(independent.var.type)){independent.var.type<-guess_type(data[[independent.var]])}
  variable.type <- paste0(dependent.var.type, "_",
                          independent.var.type)
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
    "CASE_group_difference_categorical_categorical",
    "CASE_group_difference_numerical_categorical",
    "CASE_direct_reporting_numerical_",
    "CASE_direct_reporting_categorical_",
    "CASE_direct_reporting_categorical_categorical",
    "CASE_direct_reporting_numerical_categorical"
  ))
}


is_valid_case_string<-function(x,implemented_only=T){

  return(x %in% list_all_cases(implemented_only = implemented_only))

}

case_not_implemented_error<-function(case,situation){
  stop(paste(situation,":  case", case, "not implemented"))
}



#' map to summary statistic
#'
#' selects an appropriate summary statistic function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of \code{\link{map_to_case}}. To list valid case strings use \link{\code{list_all_cases}}
#' @return a _function_ that computes the relevant summary statistic
#' @examples map_to_summary_statistic("group_difference_categorical_categorical")
#' @examples my_case<- map_to_case( ... )
#' my_sumstat <- map_to_summary_statistic(my_case)
#' my_sumstat( ... )
#' @export
map_to_summary_statistic <- function(case) {
  # sanitise input:
  if(!is_valid_case_string(case)){stop("input to map_to_summary_statistic must be a valid analysis case")}

  # define summary functions for all cases:
  summary_functions<-list()
  summary_functions$CASE_group_difference_categorical_categorical <- percent_with_confints
  summary_functions$CASE_direct_reporting_numeric_ <- confidence_intervals_num
  summary_functions$CASE_direct_reporting_categorical_ <- percent_with_confints
  # summary_functions$CASE_group_difference_numerical_numerical <- function(...){stop(paste("summary statistic for case",case,"not implemented"))}hypothesis_test_one_sample_t
  summary_functions$CASE_group_difference_numerical_categorical <- confidence_intervals_num_groups



  # return corresponding summary function:

  return(summary_functions[[case]])

}



#' map to hypothesis test
#'
#' selects an appropriate hypothesis test function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of \code{\link{map_to_case}}. To list valid case strings use \link{\code{list_all_cases}}
#' @return a _function_ that computes the relevant hypothesis test
#' @examples map_to_summary_statistic("group_difference_categorical_categorical")
#' @examples my_case<- map_to_case( ... )
#' my_hyptest <- map_to_hypothesis_test(my_case)
#' my_hyptest( ... )
#' @export
map_to_hypothesis_test <- function(case) {
  # prefill all valid cases with 'not implemented' errors:
  hypothesis_test_functions<-list()
  hypothesis_test_functions<-lapply(list_all_cases(implemented_only = F),function(x){
    function(...){stop(paste("not implemented: hypothesis test for case",x,".\n the geneva data unit can help!"))}
  })
  names(hypothesis_test_functions)<-list_all_cases(implemented_only = F)

  # add implemented cases:
  hypothesis_test_functions[["CASE_group_difference_categorical_categorical"]] <- hypothesis_test_chisquared
  hypothesis_test_functions[["CASE_direct_reporting_numerical_"]] <- hypothesis_test_empty
  hypothesis_test_functions[["CASE_direct_reporting_categorical_"]] <- hypothesis_test_empty
  hypothesis_test_functions[["CASE_group_difference_numerical_categorical"]] <- hypothesis_test_t_two_sample
  # return function belonging to this case:
  return(hypothesis_test_functions[[case]])
}


#################################
# map to visualisation:         #
#################################


#' map to visualisation
#'
#' selects an appropriate visualisation function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of \code{\link{map_to_case}}. To list valid case strings use \link{\code{list_all_cases}}
#' @return a _function_ that creates the relevant ggplot object
#' @examples map_to_visualisation("group_difference_categorical_categorical")
#' @examples my_case<- map_to_case( ... )
#' my_vis_fun <- map_to_visualisation(my_case)
#' my_ggplot_obj<-my_vis_fun( ... )
#' my_ggplot_obj # plots the object
#' @export
map_to_visualisation <- function(result) {

  invalid_input_message<-"'result' parameter not a valid hypegrammaR result object."

  if(!is.list(result)){stop(invalid_input_message)}
  if(is.null(result$parameters$case)){stop(invalid_input_message)}


  visualisation_functions<-list()
  # prefill all valid cases with 'not implemented' errors:
  visualisation_functions<-list()
  visualisation_functions<-lapply(list_all_cases(implemented_only = F),function(x){
    function(...){warning(paste("not implemented: visualisation for case",x,".\n the geneva data unit can help!"));return(NULL)}
  })
  names(visualisation_functions)<-list_all_cases(implemented_only = F)

  # add implemented cases:
  visualisation_functions[["CASE_group_difference_categorical_categorical"]] <- grouped_barchart_percent
  visualisation_functions[["CASE_group_difference_numerical_categorical"]] <- barchart_average
  visualisation_functions[["CASE_direct_reporting_categorical_"]] <- barchart_percent
  visualisation_functions[["CASE_direct_reporting_numerical_"]] <- barchart_average

  return(visualisation_functions[[result$parameters$case]](result))
}


#' Save outputs to files
#'
#' @param object The object you want to save as a file
#' @param filename The name of the file that is produced. The extension needs to match the type of object you want to save (csv for tables, jpg/pdf for images)
#' @value the object that was given as input (unchanged).
#' @examples
#' # some table:
#' mytable<-data.frame(a=1:10,b=1:10)
#' map_to_file(mytable,"mytable.csv")
#'
#' # some graphic made with ggplot:
#' mygraphic<-ggplot(mytable,aes(a,b))+geom_point()
#' map_to_file(mygraphic,"visualisation.jpg")
#' map_to_file(mygraphic,"visualisation.pdf")
#' @export
map_to_file<-function(object,filename,...){

  tryCatch({

    if("ggplot" %in% class(object)){
      ggsave(filename,object,...,limitsize = F)
      return(filename)
    }

    if("data.frame" %in% class(object)){
      write.csv(object,filename,...)
    }

  },
  error=function(e){
    logmessage(paste0("Could not write to the file called:\n",filename))
    logmessage(paste0("error:\n",e$message))
    logmessage("Please close the file if it is open in any application and make sure the folder I am trying to write to exists.")
    logmessage("to try again and continue the script, type 't'. To skip writing this file and countine the script, type 's'. To cancel the whole script, type 'c'. Then press enter.")
    whattodo<-readline("Try again (t), skip this file (s), or cancel script (c)?: ")

    if(!(whattodo %in% c("t","s","c"))){
      logmessage("invalid input. You must type 't' to Try again, 's' to skip this file or 'c' to cancel the script (otherwise I'll abort the script, equivalent to typing 'c').")
      whattodo<-readline("Try again (t), skip this file (s), or cancel script (c)?: ")
    }
    if(!(whattodo %in% c("t","s","c"))){
      stop("Could not write to a file")
    }

    if(whattodo=="t"){return(map_to_file(object,filename))}
    if(whattodo=="s"){
      logmessage("WRITING TO FILE HAS BEEN SKIPPED. Proceeding with the script.")
      return(NULL)}
    if(whattodo=="c"){stop("Could not write to a file, and user decided to cancel the script.")}

  },
  finally = {}
  )
  return(object)
}

#' Create weighting from a sampling frame
#'
#' @inheritParams surveyweights::weighting_fun_from_samplingframe
#' @details Create a 'weighter' function from a sampling frame data frame. Uses surveyweights::weighting_fun_from_samplingframe()
#' @export
map_to_weighting<-function(...){
  surveyweights::weighting_fun_from_samplingframe(...)
}


#' Create questionnaire from csv files
#'
#'
#' @details This enables new functions associated with the questionnaire. It uses load_questionnaire() from the koboquest package.
#' @inheritParams koboquest::load_questionnaire
#' @export
map_to_questionnaire<-function(...){
  koboquest::load_questionnaire(...)
}




