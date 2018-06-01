rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")

data<-load_assessment_bgd()
camps <- select_mulitpleify(data$VAR.11...enter.the.survey.site.)
data <- cbind(data, camps)
# data.plan.per.camp <- many_plans(unique(data$VAR.11...enter.the.survey.site.))
# data.plan.per.camp <- data.plan.per.camp[c(1,2)]

analysisplan<-map_to_analysis_plan_all_vars_as_dependent(each.x.as.independent.in.var = "VAR.11...enter.the.survey.site.",data = data)

results <- apply_data_analysis_plan(data,analysisplan)

result_table <- map_list_of_results_to_dataframe(results)
result_table %>% write.csv("camp_comparison.csv")


split_by_in_camp_true<-result_table %>% split.data.frame(result_table$independent.var.value)

data.frame("other camps"=split_by_in_camp_true[[1]],"this camp"=split_by_in_camp_true[[2]]$numbers) %>% write.csv("camp_comparison.csv")

