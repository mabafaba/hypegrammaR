getwd()
setwd("./GitHub/hypegrammaR")
getwd()
assessment_data<-load_data(file = "GitHub/hypegrammaR/tests/testthat/testdata.csv")

sampling_frame <- read.csv("./tests/testthat/test_samplingframe.csv", stringsAsFactors = F)

library(koboquest)
questionnaire<-load_questionnaire(data = assessment_data,
                                  questions = "./tests/testthat/kobo questions.csv",
                                  choices = "./tests/testthat/kobo choices.csv",
                                  choices.label.column.to.use = "label::English"
)

weighting <- map_to_weighting( sampling.frame = sampling_frame,
                               data.stratum.column = "stratification",
                               sampling.frame.population.column = "population",
                               sampling.frame.stratum.column = "strata.names",
                               data = assessment_data)

case <- map_to_case(hypothesis.type = "group_difference",
                    dependent.var.type = "numerical",
                    independent.var.type = "categorical")

result<-map_to_result(data = assessment_data,
                      dependent.var = "hhnumberindividuals",
                      independent.var =   "region",
                      case = case,
                      weighting = weighting)

result %>% map_to_labeled(questionnaire) -> result_labeled
chart <- result_labeled %>% map_to_visualisation


analysisplan<-load_analysisplan("./tests/testthat/analysisplan.csv")

list_of_results <-  from_analysisplan_map_to_output(data = assessment_data,
                                                    analysisplan = analysisplan,

                                                                                             weighting = weights,
                                                    questionnaire = questionnaire)
library("knitr")
list_of_results %>% map_to_template(questionnaire = questionnaire,
                                    dir = "./tests", filename = "my_example_report.html")

tab <- lapply(list_of_results$results, map_to_table)
lapply(tab,
       FUN = function(x){map_to_file(x, paste0("./tests/", names(x), "table.csv"))})

