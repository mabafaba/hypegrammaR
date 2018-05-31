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

# questionnaire<-load_questionnaire(data = "test_data/bgd/BGD_Cross_camp.csv",
#                                   questions.file = "test_data/bgd/questionnaire_BGD_UNHCR_SiteProfile_KOBO_R3b_APRIL2018.csv",
#                                   choices.file = "test_data/bgd/Choices_BGD_UNHCR_SiteProfile_KOBO_R3b_APRIL2018.csv",
#                                   choices.label.column.to.use = "english")


data<- load_data("./test_data/bgd/BGD_Cross_camp.csv")

data<- data[,-which(colnames(data)==".please.record.the.location..precision")]

data %>% str

colnames(data)<-paste0("VAR.",1:ncol(data),"...",colnames(data))
data$VAR.11...enter.the.survey.site. <- gsub(" ", ".", data$VAR.11...enter.the.survey.site.)


#####
select_mulitpleify<-function(x){
  separated<-lapply(unique(x),function(uniquex){
    x==uniquex
  }) %>% as.data.frame
  colnames(separated)<-unique(x)
  return(separated)
}

camps <- select_mulitpleify(data$VAR.11...enter.the.survey.site.)
#colnames(camps) <- paste0("VAR.", 1:ncol(camps), "...", colnames(camps))
data <- cbind(data, camps)
data %>% tail
#######


remove_duplicate_columns<-function(data){
  colnames(data)<-gsub("\\.1$","",colnames(data))
  data[,!duplicated(colnames(data))]

  }



data<-remove_duplicate_columns(data)
data<-lapply(data,function(x){x[which(x=="")]<-NA;x}) %>% as.data.frame(stringsAsFactors=F)


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

data.plan.per.camp <- many_plans(unique(data$VAR.11...enter.the.survey.site.))

data.plan.per.camp <- data.plan.per.camp[c(1,2)]


results <- lapply(data.plan.per.camp, function(y){
  results.camp <- apply(y,1,function(x){
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

  # table(list(data[[x["dependent.var"]]],data[[x["independent.var"]]])) %>% table %>% print
    var.camp <- analyse_indicator(this_valid_data,
                    dependent.var = x["dependent.var"],
                    independent.var = x["independent.var"] ,
                    # hypothesis.type =  x["hypothesis.type"],
                    sampling.strategy.cluster = FALSE,
                    sampling.strategy.stratified = TRUE,
                    case=x["case"])
    names(results.camp) <- y$dependent.var
    }
)
  })



`%£%`<- function(x,y){
  lapply(x,function(x){x[[y]]})
}


sumstatlist <- results%£%"summary.statistic"
sumstatlist



pvals<-sumstats$`p value`
pvals

false_discovery_rate(p.values){
  pvalues_sorted<-sort(pvals,decreasing = T)

}




sapply(results,getpval) %>% kable

results_to_df<-function(results){

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
    cbind(indicator=name,"p value"=getpval(results[[name]]),"test type"= results[[name]]$hypothesis.test$name, x)
  }
})  %>%  do.call(rbind,.)


}


sumstats%>% write.csv("male_vs_female.csv")



sumstatlist %>% lapply(ncol) %>% unlist %>% table
sumstats %>% lapply(colnames) %>% lapply(paste,collapse="  -  ")%>% unlist %>% table






`%do.call%` <- function(x,y){
  do.call(y,x)
}

sumstats %>% head


sumstats %>% head


hypothesis_test_t_two_sample




(data$VAR.18...what.is.your.relationship.to.the.head.of.family. =="" )%>% table



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







