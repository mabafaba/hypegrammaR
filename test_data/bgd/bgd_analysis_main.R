# DEPENDENCIES
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
source("./test_data/bgd/bgd_analysis_functions.R")


# LOAD DATA ETC
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

#####
camps <- select_mulitpleify(data$VAR.11...enter.the.survey.site.)
#colnames(camps) <- paste0("VAR.", 1:ncol(camps), "...", colnames(camps))
data <- cbind(data, camps)
#####


giantplan<-map_to_analysis_plan_all_vars_as_dependent(each.x.as.independent.in.var = "VAR.11...enter.the.survey.site.",data = data)
results<-apply_data_analysis_plan(data,giantplan[100:120,])
result_table<-map_list_of_results_to_dataframe(results)


result_table$indicator <- gsub("VAR\\.[0-9]*\\.\\.\\.","",result_table$indicator)
result_table$indicator <- gsub("\\."," ",result_table$indicator)


split_by_gender<-result_table %>% split.data.frame(result_table$independent.var.value)


gender_as_cols<-data.frame("Indicator"=split_by_gender$Female$indicator,
      "Answer choice"=split_by_gender$Female$dependent.var.value,
      "p-Value" = split_by_gender$Female$`p value`,
      "test name" = split_by_gender$Female$`test type`,
      "Number Female HHH" = split_by_gender$Female$numbers,
      "Numbers Male HHH"=split_by_gender$Male$numbers) %>% head


gender_as_cols$absolut.difference<-abs(gender_as_cols$Numbers.Male.HHH-gender_as_cols$Number.Female.HHH)

gender_as_cols %>% write.csv("male_vs_female.csv")


gender_as_cols %>% glimse
gender_as_cols %>% aggregate.data.frame(list(gender_as_cols$)




