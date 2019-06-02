`md_add_lines<-` <-function(x,value){
  value<-paste0(value,collapse = "\n")
  paste0(x,"\n",value)
}

#' Rmarkdown from resultlist in specified hierarchical order
#' @param resultlist structure like the output from from_analysisplan_map_to_output:
#' A list with two items "analysisplan" and "results": The "analysisplan" as a data frame, where each row must match a  result in a list of "results"
#' @param by_analysisplan_columns vector of strings matching column names of the analysisplan. The first element becomes the main heading, the second element the sub-heading etc.
#' @param by_prefix a prefix added at the beginnig of the headline; same length as `by_analysisplan_columns`
#' @param level the markdown header level to start with; defaults to 2 which leads to "## heading", i.e. the second header level.
#' @param render_result_with a function that takes a single result as input and returns an rmarkdown formated string
#' @param questionnaire optional; the questionnaire (koboquest::load_questionnaire())
#' @param label_varnames wether variables names should be labeled in headings
resultlist_recursive_markdown<-function(resultlist,
                       by_analysisplan_columns = c("dependent.var"),
                       by_prefix = c("", "subset:", "variable:"),
                       level = 2,
                       render_result_with,
                       questionnaire = NULL,
                       label_varnames = TRUE){



    assertthat::assert_that(is.list(resultlist))
    assertthat::assert_that("analysisplan" %in% names(resultlist))
    assertthat::assert_that("results" %in% names(resultlist))
    assertthat::assert_that(is.data.frame(resultlist$analysisplan))
    assertthat::assert_that(is.list(resultlist$results))
    assertthat::assert_that(nrow(resultlist$analysisplan) == length(resultlist$results))

    md_out<-""



    if(!is.null(questionnaire) & label_varnames){
      var_label<-questionnaire$question_get_question_label
    }else{
      var_label<-identity
    }

    by<-by_analysisplan_columns

    for(x in unique(resultlist$analysisplan[[by[1]]])){


      md_add_lines(md_out)<-paste(paste0(rep("#",level),collapse=""),by_prefix[1]," ",var_label(x))
      result_subset <- results_subset(results = resultlist,
                                 logical = resultlist$analysisplan[[by[1]]]== x)


      # if there's more 'by's left, start over for each subset
      if(length(by)>1){
        md_add_lines(md_out) <- resultlist_recursive_markdown(
                   result_subset,
                   by_analysisplan_columns = by[-1],
                   by_prefix = by_prefix[-1],
                   level = level+1,
                   render_result_with = render_result_with,
                   questionnaire = questionnaire,
                   label_varnames = label_varnames)
      }else{
        for(result in result_subset$results){
          md_add_lines(md_out) <- render_result_with(result)
        }


    }
  }
    md_out
}





#' subset a list of results based on analysis parameters
#' @param results list of results (output from `from_analysisplan_map_to_output()`)
#' @param repeat.vars optional: vector of character strings: keeps only results where repeat.var in this list
#' @param repeat.var.values optional: vector of character strings: keeps only results where repeat.var.vaues in this list
#' @param dependent.vars optional: vector of character strings: keeps only results where dependent.var in this list
#' @param logical optional: subset by a logical vector (same length as list of results)
#' @details if multiple parameters are given to subset by, only those are kept where all conditions apply
#' @return a resultlist in same format as from_analysisplan_map_to_output() only including those results with matching analysis parameters
results_subset<-function(results, repeat.vars = NULL,repeat.var.values = NULL,dependent.vars = NULL,logical = NULL){
  keep<-rep(TRUE,nrow(results$analysisplan))
  if(!is.null(logical)){
    keep[!logical]<-FALSE
  }
  if(!is.null(repeat.vars)){
    assertthat::assert_that(is.vector(repeat.vars))
    assertthat::assert_that(is.character(repeat.vars))
    keep[!(results$analysisplan$repeat.var %in% repeat.vars)]<-FALSE
  }
  if(!is.null(repeat.var.values)){
    assertthat::assert_that(is.vector(repeat.var.values))
    assertthat::assert_that(is.character(repeat.var.values))
    keep[!(results$analysisplan$repeat.var.value %in% repeat.var.values)]<-FALSE
  }
  if(!is.null(dependent.vars)){
    assertthat::assert_that(is.vector(dependent.vars))
    assertthat::assert_that(is.character(dependent.vars))
    keep[!(results$analysisplan$dependent.var %in% dependent.vars)]<-FALSE
  }
  small_results<-list()
  small_results$results<-purrr::keep(results$results,keep)
  small_results$analysisplan<-results$analysisplan[keep,]
  small_results
}



results_rbind_summary_statistic<-function(results){

  results$results %>% lapply(function(x){x$summary.statistic}) %>% do.call(rbind , .)

}



