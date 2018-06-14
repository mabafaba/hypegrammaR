## prepare workspace

# clear all objects:
rm(list=ls())
# set working directory to this script's path:
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

## load dependencies and inputs
source("./composite_indicators.R")
data<-read.csv("gws_du_exercise_example_data.csv",stringsAsFactors = F)
indicator.definition<-read.csv("./indicator_definition.csv",stringsAsFactors = F)

# calculate composite indicator
data$vulnerability.score <- compose_indicator_binarised_weighted(data,
                                                            variables = indicator.definition$variables,
                                                            critical.values = indicator.definition$critical.values,
                                                            weights = indicator.definition$weights)




# validation
hist(data$vulnerability.score)
# validation:
if(round(mean(
  data$vulnerability.score
  ,na.rm=T),7)!=0.2989104){stop("\n\n\n\n=results not correct\n\n")}else{message("\n\n\nresults correct\n\n")}
