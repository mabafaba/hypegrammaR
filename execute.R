setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
rm(list = ls())
getwd()
require("reachR")
require("data.table")
library("reachR")
library("dplyr")
library(survey)

#Execute hypegrammaR

data <- reachR:::read.csv.auto.sep("../validation/data/Kampala_AGORA_HH_Refugees_10052018.csv")
questionnaire <-  load_questionnaire(data = "../data/kri_winter.csv", 
                                     questions.file = "../data/kri_winter.csv",
                                     choices.file = "../data/choices_kri_winter.csv", 
                                    choices.label.column.to.use = "english")
populations <- load_samplingframe(sampling.frame.file = "../validation/data/Agora_Kampala_sampling_frame.csv",
                                  sampling.frame.population.column = "population", 
                                  data.stratum.column = "group",
                                  sampling.frame.stratum.column = "group",
                                  return.stratum.populations = T)

populations <- reachR:::weights_of(data)
data <- data %>%  filter(group != "#N/A")

dependent.var = "age.speaker"
independent.var = NULL

design <- map_to_design(data = data, cluster.var = NULL)
case <- map_to_case(data = data, hypothesis.type = "direct_reporting", dependent.var = "age.speaker")
analyse_indicator(data = data, dependent.var = "age.speaker", hypothesis.type = "direct_reporting")
