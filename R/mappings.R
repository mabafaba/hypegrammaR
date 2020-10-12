#' Map to Design
#'
#' creates a `survey` design object from the data
#'
#' @param data the dataset as a sampling frame. Must match the sampling frame provided to create the `weighting_function` produced with `map_to_weighting()`
#' @param weighting_function if cluster sampling was used, what's the name of the column in `data` that identifies the cluster?
#' @details create a `survey` package design object from the data and information on the sampling strategy
#' @return a `survey` package design object
#' @examples \dontrun{map_to_design(data,cluster_variable_name="cluster_id")}
#' @export
map_to_design <- function(data,
                          cluster_variable_name = NULL,
                          weighting_function = NULL) {

  # if no weighting function / cluster variable provided
  if(is.null(cluster_variable_name)){
    cluster.ids <- as.formula(c("~1"))
    }else{
    cluster.ids <- paste0("~",cluster_variable_name)
    if(any(is.na(data[cluster_variable_name]))){
  data <- data[!is.na(data[cluster_variable_name]),]
  warning("some records in the data has a missing cluster variable. These records have been removed")}
    }
  if(is.null(weighting_function)){
    strata.weights<-rep(1,nrow(data))
  }else{
    strata.weights <- weighting_function(data)
  }

  survey.design <- svydesign(data = data,
      ids = formula(cluster.ids),
      strata = names(strata.weights),
      weights = as.vector(strata.weights),nest = T)

  # attributes(survey.design)$hg_weighting_function<-ifelse(!is.null(weighting_function),weighting_function,NA)
  # attributes(survey.design)$hg_cluster_variable_name<-ifelse(!is.null(weighting_function),cluster_variable_name,NA)
  attributes(survey.design)$hg_weighting_function<-weighting_function
  attributes(survey.design)$hg_cluster_variable_name<-cluster_variable_name
    return(survey.design)

  }
#add to this an option that strata weights can be the vector of weights if there is one in the data & warning that we usually dont do this



#################################
# map to visualisation:         #
#################################


#' map to visualisation
#'
#' selects an appropriate visualisation function based on the analysis case
#'
#' @param result a result object containing the summary statistics and hypothesis tests for the case.
#' @return a _function_ that creates the relevant ggplot object
#' @examples \dontrun{map_to_visualisation("result_var1")}
#' @examples \dontrun{result_var1<- map_to_result( ... )
#' my_vis_fun <- map_to_visualisation(result_var1)
#' my_ggplot_obj<-my_vis_fun( ... )
#' my_ggplot_obj # plots the object}
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


### SHARON MAGIC
  # visualisation_functions[["CASE_group_difference_categorical_categorical"]] <- visualisationIMPACT::grouped_barchart_impact
  # visualisation_functions[["CASE_group_difference_numerical_categorical"]] <- visualisationIMPACT::grouped_boxplot_impact
  # visualisation_functions[["CASE_direct_reporting_categorical_"]] <- visualisationIMPACT::barchart_impact
  # visualisation_functions[["CASE_direct_reporting_numerical_"]] <- visualisationIMPACT::boxplot_impact
  #
 # return(visualisation_functions[[result$parameters$case]](.data = vis_input, x = vis_input$independent.var, y = vis_input$dependent.var))

  return(visualisation_functions[[result$parameters$case]](result))
  }


#' Make the master table of summary stats and hypothesis tests
#'
#' @param results_object a list containing one or more hypegrammaR result objects: the output of map_to_result
#' @param filename The name of the file that is produced. The extension needs to be ".csv".
#' @param questionnaire optional: the questionnaire obtained by load_questionnaire. Necessary is you want labeled results
#' @return a dataframe containing the summary statistics and p values for each element in results.
#' @export
map_to_master_table <- function(results_object, filename, questionnaire = NULL, ...){
  if(!is.null(questionnaire)){
    x <- map_to_labeled(result = x, questionnaire = questionnaire)}
  summary_table_single <- function(x, questions = questionnaire){
        if(!is.null(questions)){
          x <- map_to_labeled(result = x, questionnaire = questions)
        }
        y <- NULL
        no_pvalue <- is.null(x$hypothesis.test$result$p.value)
        no_hypothesis.test <- is.null(x$hypothesis.test$name)
        if(no_pvalue|no_hypothesis.test){
          x$hypothesis.test$result$p.value <- NA
          x$hypothesis.test$name <- NA
        }
        if(!is.null(x$summary.statistic)){
        y <- as.data.frame(x$summary.statistic,
             p.value = x$hypothesis.test$result$p.value,
             test.name = x$hypothesis.test$name)
        }
      return(y)
    }
    results_object <- lapply(results_object,function(x){x$summary.statistic<-as.data.frame(x$summary.statistic,stringsAsFactors=F);x})
    df <- results_object %>% lapply(summary_table_single) %>% do.call(rbind, .)
  map_to_file(df, filename)
}




#' Make the master table of summary stats
#'
#' @param results_object a list containing one or more hypegrammaR result objects: the output of map_to_result
#' @param filename The name of the file that is produced. The extension needs to be ".csv".
#' @param questionnaire optional: the questionnaire obtained by load_questionnaire. Necessary is you want labeled results
#' @return a dataframe containing the summary statistics for each element in results.
#' @export
map_to_summary_table <- function(results_object, analysisplan = NULL,
                                 filename, questionnaire = NULL,
                                 csv.type = "english"){

  summary_table_single <- function(x, questions = questionnaire, analysis_plan = analysisplan){

    y <- data.frame(dependent.var = character,
                    independent.var=character(),
                    independent.var.value=character(),
                    dependent.var.value =character(),
                    numbers=double(),
                    se=double(),
                    min=double(),
                    max=double(),
                    repeat.var=character(),
                    repeat.var.value=character(),
                    RQ=character(),
                    SRQ=character())

    if(!is.null(x$summary.statistic)){
      y <- as.data.frame(x$summary.statistic)
      y$RQ <- NA
      y$SRQ <- NA
    }

    if(!is.null(y) & !is.na(y$numbers) & (!is.null(analysis_plan))){
      print(y$dependent.var)
      y$RQ <- analysis_plan$research.question[which(analysis_plan$dependent.variable == y$dependent.var)]
      y$SRQ <- analysis_plan$sub.research.question[which(analysis_plan$dependent.variable == y$dependent.var)]
    }
    if(!is.null(questions)){
      y <- labels_summary_statistic(y, questionnaire = questions)
      y %<>% as.data.frame(.,stringsAsFactors = F)
    }
    return(y)}

  df <- results_object %>% lapply(summary_table_single) %>% do.call(rbind, .)

  if(csv.type == "french"){
    map_to_file_french(df, filename)
  }else{map_to_file(df, filename)}

}

#' Save outputs to files
#'
#' @param object The object you want to save as a file
#' @param filename The name of the file that is produced. The extension needs to match the type of object you want to save (csv for tables, jpg/pdf for images)
#' @return the object that was given as input (unchanged).
#' @examples
#' \dontrun{# some table:
#' mytable<-data.frame(a=1:10,b=1:10)
#' map_to_file(mytable,"mytable.csv")
#'
#' # some graphic made with ggplot:
#' mygraphic<-ggplot(mytable,aes(a,b))+geom_point()
#' map_to_file(mygraphic,"visualisation.jpg")
#' map_to_file(mygraphic,"visualisation.pdf")}
#' @export
map_to_file<-function(object,filename,...){

  tryCatch({

    if("ggplot" %in% class(object)){
      ggsave(filename,object,...,limitsize = F)
      return(filename)
    }

    if("data.frame" %in% class(object)){
      write.csv(object,filename)
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

#' Save outputs to files (french csv format)
#'
#' @param object The object you want to save as a file
#' @param filename The name of the file that is produced. The extension needs to match the type of object you want to save (csv for tables, jpg/pdf for images)
#' @return the object that was given as input (unchanged).
#' @examples
#' \dontrun{# some table:
#' mytable<-data.frame(a=1:10,b=1:10)
#' map_to_file(mytable,"mytable.csv")
#'
#' # some graphic made with ggplot:
#' mygraphic<-ggplot(mytable,aes(a,b))+geom_point()
#' map_to_file(mygraphic,"visualisation.jpg")
#' map_to_file(mygraphic,"visualisation.pdf")}
#' @export
map_to_file_french <-function(object,filename,...){

  tryCatch({

    if("ggplot" %in% class(object)){
      ggsave(filename,object,...,limitsize = F)
      return(filename)
    }

    if("data.frame" %in% class(object)){
      write.csv2(object,filename)
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
#' \dontrun{# load data and sampling frames:
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
#' subset_weights<- weighting(mydata)}
#' @export
map_to_weighting<-function(sampling.frame, data.stratum.column, sampling.frame.population.column = "population",
                           sampling.frame.stratum.column = "stratum", data = NULL){
  surveyweights::weighting_fun_from_samplingframe(sampling.frame = sampling.frame,
                                                  data.stratum.column = data.stratum.column,
                                                  sampling.frame.population.column = sampling.frame.population.column,
                                                  sampling.frame.stratum.column = sampling.frame.stratum.column, data = data)
}


#' Combine weight functions from two sampling frames
#'
#' @param weight_function_1 first weighthing function
#' @param weight_function_2 second weightng function
#' @return returns a new function that takes a data frame as input returns a vector of weights corresponding to each row in the data frame.
#' @export
combine_weighting_functions<-function(weight_function_1, weight_function_2){
surveyweights::combine_weighting_functions(weight_function_1, weight_function_2)
}

#' presentable p-value format
#' @export
label_pvalue <- function(x, digits = 3){
  if (x < 10^-digits) return(paste('<', 10^-digits))
  paste('=', myround(x, digits))
}

