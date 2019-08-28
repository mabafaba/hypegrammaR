
#' results as a table
#'
#' @param result a hypegrammaR `result` object produced by map_to_result
#' @return a date frame with only the summary statistics
#' @export
map_to_table<-function(result){
  # extract core information
  if(is.null(result)){return(result)}
  if(is.null(result$parameters$case)){
    warning("result object has no parameters - can't map to table");
    return(result$summary.statistic)}
  is_percent<-grepl("\\_categorical\\_",result$parameters$case)
  has_confints<-!all(is.na(c(result$summary.statistic$min,result$summary.statistic$max)))
  has_dependent_var_values<-!all(is.na(result$summary.statistic$dependent.var.value))
  has_independent_var_values<-!all(is.na(result$summary.statistic$independent.var.value))
  result$summary.statistic$numbers<-as.numeric(result$summary.statistic$numbers)

  # format numbers (add confints if available):
  # four cases percent or not, confints or not
  nums_formated<-round(result$summary.statistic$numbers,2)
  if(is_percent){nums_formated<-paste0(round(nums_formated*100),"%")}
  if(!is_percent){nums_formated<-as.character(round(nums_formated,2))}

  if(has_confints){
    if(is_percent){
    min_formated<-paste0(round(result$summary.statistic$min*100),"%")
    max_formated<-paste0(round(result$summary.statistic$max*100),"%")
    }else{
      min_formated<-paste0(round(result$summary.statistic$min,2),"")
      max_formated<-paste0(round(result$summary.statistic$max,2),"")
    }
    interval<-paste0("(",min_formated,"-",max_formated,")")
  }else{
    interval<-""
  }

    formated_nums<-paste(nums_formated,interval)
    formated_nums[formated_nums=="0 (0-0)"]<-"0"
    formated_nums[formated_nums=="0% (0%-0%)"]<-"0%"

  result$summary.statistic$numbers<-formated_nums



  # subset columns to display
  cols_to_keep<-c("numbers")
  if(has_dependent_var_values){cols_to_keep<-c(cols_to_keep,"dependent.var.value")}
  if(has_independent_var_values){cols_to_keep<-c(cols_to_keep,"independent.var.value")

  }

  table_to_show<-result$summary.statistic[,cols_to_keep,drop = FALSE]
  if(has_dependent_var_values){
    table_to_show<-tidyr::spread(table_to_show,key = 'dependent.var.value',value = numbers)
  }else{
    colnames(table_to_show)[colnames(table_to_show)=='numbers']<-ifelse(is_percent,"%","Average")
  }
  if(has_independent_var_values){
    colnames(table_to_show)[colnames(table_to_show)=='independent.var.value']<-"Subset"
    }

  rownames(table_to_show)<-NULL

  return(table_to_show)
}


map_to_methodology_description<-function(result){
significiant<-result$hypothesis.test$result$p.value<0.05

text<-paste("The analysis showed ",
            ifelse(significiant,"a","no"),"significant difference", "(hypothesis test used:",result$hypothesis.test$name,")")
}



