from_result_map_to_md_table <-function(result){
  md_out<-""

  if(!is.null(result$summary.statistic)){

    # if(!is.null(questionnaire)){
    #   result$parameters$dependent.var.label<-questionnaire$question_get_question_label(result$parameters$dependent.var)
    # }
    # cat("####", result$parameters$dependent.var.label, "\n\n")
    if(!is.null(result$parameters$independent.var)){
      md_add_lines(md_out) <- (paste("by", result$parameters$independent.var))}


    table <- result %>% (hypegrammaR:::map_to_table)
    md_add_lines(md_out) <- knitr::kable(table, format = "html") %>% kable_styling()

  }
  if(!is.null(result$hypothesis.test$result$p.value)){
    md_add_lines(md_out) <- c(as.character(unique(result$hypothesis.test$name)), "P Value:")
    result$hypothesis.test$result$p.value %>% print}
  md_add_lines(md_out) <- knitr::kable(result$hypothesis.test %>% as.data.frame,format="html")

  md_out
}
