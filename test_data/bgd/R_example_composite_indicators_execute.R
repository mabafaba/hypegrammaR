##################
### This script executes the composite indicator calculation,
### using the functions defined in "R_example_composite_indicator_functions.R"

#################
###Load and prepare the data
rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
getwd()
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")
source("./test_data/bgd/R_example_composite_indicator_functions.R")

#################
data<-read.csv("gws_du_exercise_example_data.csv")
indicator.definition<-read.csv("./indicator_definition.csv")
print(indicator.definition)

#################
### calculate the indicator
composite.indicator <- compose_indicator_weighted_binary(data,
                                                            variables = indicator.definition$variables,
                                                            critical.values = indicator.definition$critical.values,
                                                            weights = indicator.definition$weights)
# add to the dataframe
data <- cbind(data, composite.indicator)

#################
### Visualise results
data$composite.indicator %>% table
hist(data$composite.indicator, breaks = 12)
mean(data$composite.indicator)
