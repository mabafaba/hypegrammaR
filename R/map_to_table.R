

#' results as a table
#'
#' @param result a hypegrammaR `result` object produced by map_to_result
#' @value a date frame with only the summary statistics
#' @export
map_to_table<-function(result){
result$summary.statistic
  is_percent<-grepl("\\_categorical\\_",result$parameters$case)
  numbers_column_label<-ifelse(is_percent,"%","Average")
  has_confints<-!all(is.na(c(result$summary.statistic$min,result$summary.statistic$max)))
  interval<-if(has_confints){
    formated_nums<-paste0(round(result$summary.statistic$numbers,2),
                " (",round(result$summary.statistic$min,2)," - ", round(result$summary.statistic$max,2),")")

  }else{
    formated_nums<-round(result$summary.statistic$numbers,2)
  }
  include_independent_var_value<-grepl("_group_difference_",result$parameters$case)
  mytable<-data.frame(nums=formated_nums)
  colnames(mytable)<-numbers_column_label
  if(include_independent_var_value){
    mytable$Group <- result$summary.statistic$independent.var.value
  }
  return(mytable)
}


map_to_methodology_description<-function(result){
significiant<-result$hypothesis.test$result$p.value<0.05

text<-paste("The analysis showed ",
            ifelse(significiant,"a","no"),"significant difference", "(hypothesis test used:",result$hypothesis.test$name,")")
}



