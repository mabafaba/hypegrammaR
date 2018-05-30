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

# data<- data[,-which(colnames(data)=="what.is.your.relationship.to.the.head.of.family.")]


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

analysisplan %>% head
analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]

results<-apply(analysisplan,1,function(x){

  this_valid_data<-data[
      which(
        !(is.na(data[,x["dependent.var"]])) &
         (!(is.na(data[,x["independent.var"]])))
        ),
      ]

  if(length(grep("group_difference",x["case"])) > 0 ){
    if(length(table(this_valid_data[,x["independent.var"]])) < 2 |
       length(table(this_valid_data[,x["dependent.var"]])) < 2 |
       length(unique(this_valid_data[,x["dependent.var"]]))==nrow(this_valid_data)){
      return("depvar or indepvar less than 2 factors")
    }
  }
  # print(table(this_valid_data[,x["dependent.var"]]))
  # print(table(this_valid_data[,x["independent.var"]]))
  # print(x["independent.var"])
  # print(x["dependent.var"])
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



results$VAR.29...what.is.the.main.source.of.water.for.your.family.

summarystat_dfs<-lapply(names(results),function(x){
  print(x)
  if(is.list(results[[x]]))
  reformat<-results[[x]]$summary.statistic %>% as.data.frame %>% data.frame(
    rep(results[[x]]$hypothesis.test.result$test.results[2],nrow(results[[x]]$summary.statistic %>% as.data.frame)) %>% as.data.frame

  )else{
    reformat<-NULL
  }


  })


results3

  results3<-lapply(summarystat_dfs,function(x){
  if(is.data.frame(x)){if(nrow(x)!=0){x}else{NULL}}else{NULL}})

results3 %>% lapply(function(x)colnames)


lapply(1:nrow(analysisplan),function(x){
  if(analysisplan$case[x]=="CASE_group_difference_categorical_categorical")
    print((summarystat_dfs[[x]]$summary.statistic %>% as.data.frame %>% colnames))
})


results3 %>% lapply(function(x){colnames(x)==c("independent.var.value","dependent.var.value","numbers","se","min","max",".") }) %>% lapply(all)

results3[[250]] %>% colnames


table(list(data[["VAR.20...what.is.the.gender.of.the.head.of.the.family."]],
           data[["VAR.26...what.is.the.primary.reason.for.planning.to.leave.this.shelter..1"]]))


sanitise_data %>% debug
sanitise_group_difference %>% debug
sanitise_data(data = data,dependent.var = "VAR.26...what.is.the.primary.reason.for.planning.to.leave.this.shelter..1",
                independent.var = "VAR.20...what.is.the.gender.of.the.head.of.the.family.",case = "CASE_group_difference_categorical_categorical")

table(data[["VAR.26...what.is.the.primary.reason.for.planning.to.leave.this.shelter..1"]])


table("VAR.26...what.is.the.primary.reason.for.planning.to.leave.this.shelter..1")


data[110,]
table(data[1:109,1:2])
table(data[1:109,1:2])
data[c(108,109,110,112),]



datasubset<-data[c(90:110),1:2]
datasubset<-data.frame(.please.record.the.location..precision=as.character(data[[".please.record.the.location..precision"]])

                       )
rownames(datasubset)<-c(1:111)


independent.var
"what.is.the.gender.of.the.head.of.the.family."
dependent.var
"what.is.the.primary.reason.for.planning.to.leave.this.shelter."

debug(percent_with_confints)
analyse_indicator(data,
                  dependent.var ="VAR.19...what.is.your.relationship.to.the.head.of.family.",
                  independent.var = "VAR.20...what.is.the.gender.of.the.head.of.the.family." ,
                  # hypothesis.type =  x["hypothesis.type"],
                  sampling.strategy.cluster = FALSE,
                  sampling.strategy.stratified = FALSE,
                  case="CASE_group_difference_categorical_categorical"
)






data$.please.record.the.location..precision <-sample(c("a","b","c"),nrow(data),T)



undebug(percent_with_confints)
grep("what.is.your.family.s.first.priority.need.",colnames(data),value=T)

doublescolnames(data) %>% table






data$what.is.your.relationship.to.the.head.of.family..1==data$what.is.your.relationship.to.the.head.of.family.
  data$what.is.your.relationship
  grep("what.is.your.relationship.to.the.head.of.family..1$",c("what.is.your.relationship.to.the.head.of.family..1","asdf"))
"what.is.your.relationship.to.the.head.of.family..1" %in% colnames(data)

grep("what.is.your.relationship.to.the.head.of.family.",colnames(data))


