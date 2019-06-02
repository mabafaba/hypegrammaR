map_to_datamerge<-function(results,
                                      rows=c("repeat.var","repeat.var.value"),
                                      values="numbers",
                                      ignore=c("se","min","max"),
                                      questionnaire=NULL){
  # rbind all summary statistics
  all_summary_statistics <-  results %>%
    lapply(function(x){
      x$summary.statistic %>% lapply(function(x){
        if(is.factor(x)){return(as.character(x))};x}) %>% as.data.frame(stringsAsFactors=F)
    }) %>%
    do.call(rbind,.)
  if(!is.null(questionnaire)){
  all_summary_statistics_labeled<-results %>% lapply(map_to_labeled,questionnaire) %>% lapply(function(x){x$summary.statistic}) %>%
    do.call(rbind,.)
  }else{all_summary_statistics_labeled<-all_summary_statistics}
  if(nrow(all_summary_statistics)<nrow(all_summary_statistics_labeled)){
    warning("labelising made some analysis definition indistinguishable (identical question labels or same label for different choices in the same question?")
    .write_to_log("mapping resultlist to datamerge csv could not be done correctly with labels - some analysis definitions became indistinguishable ")
  }

  columns<-  names(all_summary_statistics)[!(names(all_summary_statistics) %in% c(rows,ignore,values))]
#
#   if(labelise.varnames){
#     all_summary_statistics_labeled$master_table_column_name<-  all_summary_statistics_labeled[,columns] %>% as.list %>% c(sep=":::") %>% do.call(paste,.)
#   }else{
    all_summary_statistics_labeled$master_table_column_name<-  all_summary_statistics[,columns] %>% as.list %>% c(sep=":::") %>% do.call(paste,.)

  # }


  # what to keep rows for:
  wide_format<-all_summary_statistics_labeled %>% unique %>% .[,c(rows,"master_table_column_name",values)] %>%
    spread(key = master_table_column_name,value = numbers)
  return(wide_format)
}
