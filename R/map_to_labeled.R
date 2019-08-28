#' Add labels to results
#'
#' @param result hypegrammaR `result` object; output from map_to_result().
#' @param questionnaire koboquest `questionnaire` object; output from load_questionnaire()
#' @return same as `result` input, but with all variable values labeled
#' @details if the variable wasn't found in the questionnaire, or the choice wasn't found in the corresponding list of choices, the affected values will remain unchanged.
#' @export
map_to_labeled<-function(result, questionnaire){
  if(is.null(result$summary.statistic)){return(result)}

  result$summary.statistic<-labels_summary_statistic(result$summary.statistic,
                                                     questionnaire = questionnaire,
                                                     label.dependent.var.value = T,
                                                     label.independent.var.value = T)
  return(result)
}


#' Add labels to results
#'
#' @param result hypegrammaR `result` object; output from map_to_result().
#' @param questionnaire koboquest `questionnaire` object; output from load_questionnaire()
#' @return same as input, but with all variable values labeled
#' @details if the Variable wasn't found in the questionnaire, or the choice wasn't found in the corresponding list of choices, the affected values will remain unchanged.
labels_summary_statistic<-function(summary.statistic,questionnaire,label.dependent.var.value=T,label.independent.var.value=T,label.dependent.var=T,label.independent.var=T,independent.linebreak=T,dependent.linebreak=F){
  if(is.null(summary.statistic)){return(summary.statistic)}

  if(length(unique(summary.statistic[,"dependent.var"]))>1){stop("labels_summary_statistic only works for a single combination of dependent and independent variable.")}
  if(length(unique(summary.statistic[,"independent.var"]))>1){stop("labels_summary_statistic only works for a single combination of dependent and independent variable.")}


  linebreak <- function(text,n=40) {
    wtext<-sapply(text,function(x){paste(strwrap(x,width=n),collapse="\n")})
    return(unname(wtext))
  }

  if(label.dependent.var.value){
    summary.statistic[,"dependent.var.value"]<-questionnaire$question_get_choice_labels(summary.statistic[,"dependent.var.value"],
                                                                          summary.statistic[,"dependent.var"][1])
    if(dependent.linebreak){
      summary.statistic[,"dependent.var.value"] %<>% linebreak
    }
  }

  if(label.independent.var.value){
    summary.statistic[,"independent.var.value"]<-questionnaire$question_get_choice_labels(summary.statistic[,"independent.var.value"],
                                                                            summary.statistic[,"independent.var"][1])
    if(independent.linebreak){
      summary.statistic[,"independent.var.value"] %<>% linebreak
    }

  }
  if(label.dependent.var){
    summary.statistic[,"dependent.var"]<-questionnaire$question_get_question_label(summary.statistic[,"dependent.var"])
  }
  if(label.independent.var){
    summary.statistic[,"independent.var"]<-questionnaire$question_get_question_label(summary.statistic[,"independent.var"])
  }
  return(summary.statistic)
}
