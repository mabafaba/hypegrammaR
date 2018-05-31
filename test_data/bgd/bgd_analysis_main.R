# empty wd
rm(list=ls())
require("dplyr")
require("reachR")
require("data.table")
require("knitr")
require("survey")

this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
getwd()
script_path<-"./R"

script_files<-paste(script_path,list.files(script_path),sep="/")
sapply(script_files,source)
sf<-load_samplingframe(sampling.frame.file = "test_data/bgd/Sampling_frame.csv",
                       sampling.frame.population.column = "households",
                       sampling.frame.stratum.column  = "camp.name",
                       data.stratum.column = "VAR.11...enter.the.survey.site.",
                       return.stratum.populations = T)
#
# questionnaire<-load_questionnaire(data = "test_data/bgd/BGD_Cross_camp.csv",
#                                   questions.file = "test_data/bgd/questionnaire_BGD_UNHCR_SiteProfile_KOBO_R3b_APRIL2018.csv",
#                                   choices.file = "test_data/bgd/Choices_BGD_UNHCR_SiteProfile_KOBO_R3b_APRIL2018.csv",
#                                   choices.label.column.to.use = "english")


data<- load_data("./test_data/bgd/BGD_Cross_camp.csv")

data<- data[,-which(colnames(data)==".please.record.the.location..precision")]

colnames(data)<-paste0("VAR.",1:ncol(data),"...",colnames(data))

remove_duplicate_columns<-function(data){
  colnames(data)<-gsub("\\.1$","",colnames(data))
  data[,!duplicated(colnames(data))]

  }



data<-remove_duplicate_columns(data)

analysisplan<-data.frame(
    repeat.for="VAR.11...enter.the.survey.site.",
    independent.var="VAR.20...what.is.the.gender.of.the.head.of.the.family.",
    dependent.var=names(data),
    hypothesis.type="group_difference",
    case=paste0("CASE_group_difference_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
  ,stringsAsFactors = F)

analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]


results<-apply(analysisplan,1,function(x){

  this_valid_data<-data[
      which(
        !(is.na(data[,x["dependent.var"]])) &
         (!(is.na(data[,x["independent.var"]])))
        ),
      ]

  # if(length(grep("group_difference",x["case"])) > 0 ){
  #   if(length(table(this_valid_data[,x["independent.var"]])) < 2 |
  #      length(table(this_valid_data[,x["dependent.var"]])) < 2 |
  #      length(unique(this_valid_data[,x["dependent.var"]]))==nrow(this_valid_data)){
  #     return("depvar or indepvar less than 2 factors")
  #   }
  # }


  # print(table(this_valid_data[,x["dependent.var"]]))
  # print(table(this_valid_data[,x["independent.var"]]))
  # print(x["independent.var"])
  print(x["dependent.var"])
  # table(list(data[[x["dependent.var"]]],data[[x["independent.var"]]])) %>% table %>% print
    analyse_indicator(this_valid_data,
                    dependent.var = x["dependent.var"],
                    independent.var = x["independent.var"] ,
                    # hypothesis.type =  x["hypothesis.type"],
                    sampling.strategy.cluster = FALSE,
                    sampling.strategy.stratified = TRUE,
                    case=x["case"]
                    )
       })



names(results)<-analysisplan$dependent.var




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


map_list_of_results_to_dataframe<-function(analysis_indicator_results){

  `%£%`<- function(x,y){
    lapply(x,function(x){x[[y]]})
  }


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



result_table<-map_list_of_results_to_dataframe(results)
result_table$indicator <- gsub("VAR\\.[0-9]*\\.\\.\\.","",result_table$indicator)
result_table$indicator <- gsub("\\."," ",result_table$indicator)


split_by_gender<-result_table %>% split.data.frame(result_table$independent.var.value)
split_by_gender


gender_as_cols<-data.frame("Indicator"=split_by_gender$Female$indicator,
      "Answer choice"=split_by_gender$Female$dependent.var.value,
      "p-Value" = split_by_gender$Female$`p value`,
      "test name" = split_by_gender$Female$`test type`,
      "Number Female HHH" = split_by_gender$Female$numbers,
      "Numbers Male HHH"=split_by_gender$Male$numbers) %>% head



gender_as_cols$absolut.difference<-abs(gender_as_cols$Numbers.Male.HHH-gender_as_cols$Number.Female.HHH)





sumstats%>% write.csv("male_vs_female.csv")



sumstatlist %>% lapply(ncol) %>% unlist %>% table
sumstats %>% lapply(colnames) %>% lapply(paste,collapse="  -  ")%>% unlist %>% table


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
