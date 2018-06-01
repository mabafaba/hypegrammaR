rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")

data<-load_assessment_bgd()


analysisplan<-data.frame(
  repeat.for="VAR.11...enter.the.survey.site.",
  independent.var="VAR.20...what.is.the.gender.of.the.head.of.the.family.",
  dependent.var=names(data),
  hypothesis.type="group_difference",
  case=paste0("CASE_group_difference_",ifelse(data %>% sapply(is.numeric),"numerical","categorical"),"_categorical")
  ,stringsAsFactors = F)

analysisplan <- analysisplan[analysisplan[,"dependent.var"]!= analysisplan[,"independent.var"],]


results<-apply_data_analysis_plan(data,analysisplan)


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




result_table %>% write.csv("gender_comparison.csv")

