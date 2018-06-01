# DEPENDENCIES
# empty wd
rm(list=ls())
require("dplyr")
require("reachR")
require("data.table")
require("knitr")
require("survey")

script_path<-"./R"

script_files<-paste(script_path,list.files(script_path),sep="/")
sapply(script_files,source)
source("./test_data/bgd/bgd_analysis_functions.R")

# LOAD DATA ETC

load_assessment_bgd<-function(){

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

    # PREP DATA

    data<- data[,-which(colnames(data)==".please.record.the.location..precision")]
    data<-lapply(data,function(x){x[which(x=="")]<-NA;x}) %>% as.data.frame(stringsAsFactors=F)
    colnames(data)<-paste0("VAR.",1:ncol(data),"...",colnames(data))
    data$VAR.11...enter.the.survey.site. <- gsub(" ", ".", data$VAR.11...enter.the.survey.site.)
    return(data)
}
