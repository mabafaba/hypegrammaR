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

undebug(percent_with_confints)
undebug(sanitise_group_difference)

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




results %>% lapply(function(x){x$message}) %>% unlist %>% table %>% kable





results %>% lapply(function(x){if(length(x)==4){print(names(x))}})
results %>% lapply(nbam) %>% unlist %>% table

results %>% lapply(function(x){x$hypothesis.test.result}) %>% lapply(names) %>% lapply(paste,collapse=":::") %>% unlist %>% table


results %>% lapply(function(x){x$hypothesis.test.result}) %>% lapply(is.null) %>% unlist %>% table




results %>% lapply(function(x){x$summary.statistic}) %>% lapply(if(length(x)==)) %>% unlist %>% table
results %>% lapply(function(x){x$summary.statistic}) %>% lapply(function(x){if(is.list(x)){colnames(x) %>% paste(collapse=":::")}}) %>% unlist %>% table

results %>% lapply(function(x))



results$`260`

results$`257`$summary.statistic
results$`258`$summary.statistic


results %>% lapply(function(x){x$summary.statistic}) %>% lapply(length) %>% unlist %>% table

results %>% lapply(function(x){x$summary.statistic}) %>% lapply(names) %>% lapply(paste,collapse=":::") %>% unlist %>% table






results$`239` %>% names
























# summarystat_dfs<-lapply(names(results),function(x){
#   print(x)
#   if(is.list(results[[x]]))
#   reformat<-results[[x]]$summary.statistic %>% as.data.frame %>% data.frame(
#     rep(results[[x]]$hypothesis.test.result$test.results[2],nrow(results[[x]]$summary.statistic %>% as.data.frame)) %>% as.data.frame
#
#   )else{
#     reformat<-NULL
#   }
#
#
#   })
debug(sanitise_group_difference)
analyse_indicator(data,
                  dependent.var ="VAR.5...hello.my.name.is.........i.work.for.reach..together.with.unhcr..we.are.currently.conducting.a.survey.to.understand.the.needs.of.refugees.from.myanmar..we.would.like.to.know.more.about.the.needs.of.your.family.and.to.what.services.you.have.access..we.also.may.ask.you.a.few.questions.about.yourself.personally..the.survey.usually.takes.between.30.and.45.minutes.to.complete..any.information.that.you.provide.will.be.kept.anonymous..this.is.voluntary.and.you.can.choose.not.to.answer.any.or.all.of.the.questions.if.you.want..you.may.also.choose.to.quit.at.any.point..however..we.hope.that.you.will.participate.since.your.views.are.important..participation.in.the.survey.does.not.have.any.impact.on.whether.you.or.your.family.receive.assistance..do.you.have.any.questions..may.i.begin.now.",
                  independent.var = "VAR.11...enter.the.survey.site." ,
                  # hypothesis.type =  x["hypothesis.type"],
                  sampling.strategy.cluster = FALSE,
                  sampling.strategy.stratified = FALSE,
                  case="CASE_group_difference_categorical_categorical"
)






ct<-function(...){
  table(list(...))
}
ctdi<-function(){
  ct(data[[dependent.var]],data[[independent.var]])
}

