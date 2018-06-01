select_mulitpleify<-function(x){
  separated<-lapply(unique(x),function(uniquex){
    x==uniquex
  }) %>% as.data.frame
  colnames(separated)<-unique(x)
  return(separated)
}


remove_duplicate_columns<-function(data){
  colnames(data)<-gsub("\\.1$","",colnames(data))
  data[,!duplicated(colnames(data))]

}







apply_data_analysis_plan<-function(data,analysisplan){

   results<- apply(analysisplan,1,function(x){
      this_valid_data<-data[
        which(
          !(is.na(data[,x["dependent.var"]])) &
            (!(is.na(data[,x["independent.var"]])))
        ),
        ]

      analyse_indicator(this_valid_data,
                        dependent.var = x["dependent.var"],
                        independent.var = x["independent.var"] ,
                        # hypothesis.type =  x["hypothesis.type"],
                        sampling.strategy.cluster = FALSE,
                        sampling.strategy.stratified = TRUE,
                        case=x["case"])
    }
    )

  names(results)<-analysisplan$dependent.var
  return(results)

}



false_discovery_rate<-function(p.values,q=0.05){
  # FDR according to Benjamini and Hochberg 1994
  # http://engr.case.edu/ray_soumya/mlrg/controlling_fdr_benjamini95.pdf
  p_i<-sort(p.values,decreasing = F)
  m<-length(p_i)
  i<-1:length(p_i)
  critical_ps_per_i<-i/m*q
  smaller_than_critical_per_i<- p_i < critical_ps_per_i
  if(!smaller_than_critical_per_i[1]){return(0)}
  if(all(smaller_than_critical_per_i)){return(1)}

  first_not_smaller_than_critical<-min(which(!smaller_than_critical_per_i))
  k<-first_not_smaller_than_critical-1
  p_k<-p_i[k]
  return(p_k)
}


`%£%`<- function(x,y){
  lapply(x,function(x){x[[y]]})
}


map_list_of_results_to_dataframe<-function(analysis_indicator_results){


  sumstatlist <- analysis_indicator_results%£%"summary.statistic"



  getpval<-function(result){
    if(is.null(result)){return(NA)}
    if(!is.list(result)){return(NA)}
    if(is.null(result$hypothesis.test)){return(result$message)}
    if(is.null(result$hypothesis.test$result)){return(result$message)}
    if(is.null(result$hypothesis.test$result$p.value)){return(result$message)}

    return(result$hypothesis.test$result$p.value)
  }

  sumstats <- nameapply(sumstatlist,function(x,name){


    if (!is.null(x)) {
      cbind(indicator=name,"p value"=getpval(analysis_indicator_results[[name]]),"test type"= analysis_indicator_results[[name]]$hypothesis.test$name, x)
    }
  })  %>%  do.call(rbind,.)

  return(sumstats)
}



rbind.list<-function(x){
  do.call(rbind,x)
}

nameapply<-function(x,FUN,...){
  names<-names(x)
  lapply(names,function(name){
    x<-x[[name]]
    # FUN(x,name=name,...)
    do.call(FUN,list(x=x,name=name,...))
  })
}





map_to_analysis_plan_all_vars_as_dependent <- function(each.x.as.independent.in.var,data){
  x<-unique(data[[each.x.as.independent.in.var]])
  lapply(x, function(indep.var){
    analysisplan <-data.frame(independent.var= indep.var,
                              dependent.var=names(data),
                              hypothesis.type="group_difference",
                              case=paste0("CASE_group_difference_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
                              ,stringsAsFactors = F)
    analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
    return(analysisplan)
  }) %>% do.call(rbind,.)
}




many_plans <- function(x){
  lapply(x, function(indep.var){
    analysisplan <-data.frame(independent.var= indep.var,
                              dependent.var=names(data),
                              hypothesis.type="group_difference",
                              case=paste0("CASE_group_difference_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
                              ,stringsAsFactors = F)
    analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]
    return(analysisplan)
  })
}
