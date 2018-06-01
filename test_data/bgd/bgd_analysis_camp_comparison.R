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

results <- apply_data_analysis_plan(data,analysisplan[1:20,])

result_table <- map_list_of_results_to_dataframe(results)
result_table %>% write.csv("camp_comparison.csv")

# result_table %>% aggregate.data.frame(by=list(result_table$indicator,result_table$independent.var.value),FUN = max)








maxORmode<-function (x, when.tie = NA, mean.when.fraction.numeric.greater.than = 1)
{
  if (is.numeric.fuzzy(as.character(x), minfrac = mean.when.fraction.numeric.greater.than)) {
    return(suppressWarnings(x %>% as.character %>% as.numeric %>%
                              hasdata %>% median))
  }
  else {
    return(x %>% hasdata %>% Mode(when.tie = when.tie))
  }
}
