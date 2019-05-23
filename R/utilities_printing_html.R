rmdrs_pretty_summary_table<-function(summary.table){

  # format for export
  # <-c(
  #   results$results[[mycount]]$input.parameters$repeat.var,
  #   results$results[[mycount]]$input.parameters$dependent.var,
  #   results$results[[mycount]]$input.parameters$independent.var,
  #   "numbers",
  #   "se",
  #   "lover conf. limit",
  #   "upper conf. limit"
  # )

  df_rename<-function(df,from,to,order=NULL){

    for(i in 1:length(from)){
      df[[to[i]]]<-df[[from[i]]]
      df[[from[i]]]<-NULL
    }
    inorder<-to[order];inorder<-inorder[inorder%in%colnames(df)]
    notinorder<-names(df)[!(names(df) %in% inorder)]
    df[,c(inorder,notinorder)]
  }

  kdf<-df_rename(summary.table,from=c("min",
                            "max",
                            "numbers",
                            "independent.var",
                            "independent.var.value",
                            "dependent.var",
                            "repeat.var",
                            "repeat.var.value",
                            "dependent.var.value"),
                 to= c("Lower confidence limit",
                       "Upper confidence limit",
                       "Summary statistic",
                       "Independent variable",
                       "Value of independent variable",
                       "Variable",
                       "Subset variable",
                       "Subset",
                       "Response"),
                 order=c(8,
                         9,
                         7,
                         5,
                         6,
                         3,
                         1,
                         2,
                         4)
  )
  kdf<-kdf[,which(lapply(kdf,function(x){!all(is.na(x))}) %>% unlist)]
  names(kdf)<-colnames(kdf)


  return(kdf)
}
