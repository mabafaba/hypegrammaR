#' Map to Design
#'
#' creates a `survey` design object from the data
#'
#' @param data the dataset as a sampling frame. Must match the sampling frame provided to create the `weighting_function` produced with `map_to_weighting()`
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






#################################
# map to visualisation:         #
#################################


#' map to visualisation
#'
#' selects an appropriate visualisation function based on the analysis case
#'
#' @param case a string uniquely identifying the analysis case. output of map_to_case(). To list valid case strings use hypegrammar::list_all_cases()
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
#' @return the object that was given as input (unchanged).
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

#' creates a weighting function from a sampling frame
#'
#' @param sampling.frame.file data frame containing the sampling frame. should contain columns "stratum" and "population", otherwise column names must be specified.
#' @param sampling.frame.population.column sampling frame name of column holding population counts. defaults to "population"
#' @param sampling.frame.stratum.column sampling frame name of column holding stratum names. defaults to "stratum". Stratum names must match exactly values in:
#' @param data.stratum.column data column name that holds the record's strata names
#' @param data optional but recommended: you can provide an example data frame of data supposed to match the sampling frame to check if the provided variable names match and whether all strata in the data appear in the sampling frame.
#' @return returns a new function that takes a data frame as input returns a vector of weights corresponding to each row in the data frame.
#' @examples
#' # laod data and sampling frames:
#' mydata<-read.csv("mydata.csv")
#' mysamplingframe<-read.csv("mysamplingframe.csv")
#' # create weighting function:
#' weighting<-weighting_fun_from_samplingframe(sampling.frame = mysamplingframe,
#'                                  data.stratum.column = "strata_names",
#'                                  sampling.frame.population.column = "pop",
#'                                  sampling.frame.stratum.column = "strat_name")
#' # use weighting function:
#' mydata$weights<-weighting(mydata)
#'
#' # this also works on subsets of the data:
#' mydata_subset<-mydata[1:100,]
#' subset_weights<- weighting(mydata)
#' @export
map_to_weighting<-function(sampling.frame, data.stratum.column, sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "stratum", data = NULL){
  surveyweights::weighting_fun_from_samplingframe(sampling.frame = sampling.frame,
                                                  data.stratum.column = data.stratum.column,
                                                  sampling.frame.population.column = sampling.frame.population.column,
                                                  sampling.frame.stratum.column = sampling.frame.stratum.column)
}


#' presentable p-value format
#' @export
label_pvalue <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', myround(x, digits))
}

