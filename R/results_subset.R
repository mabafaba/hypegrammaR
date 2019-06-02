
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

