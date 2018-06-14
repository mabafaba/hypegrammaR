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

data<-load_assessment_bgd()
################# 

################# 
###Make a list of variables entering the composite indicator with their relative weights
###This can also be replaced by loading a csv file with the parameters, values and weights

parameters <- c("VAR.58...what.is.are.the.problem.s..related.to.the.latrine.if.any..it.is.not.safe..no.door..no.lock..etc..",
"VAR.99...does.the.shelter.door.have.a.lock.for.the.shelter.to.be.locked.from.the.inside.",
"VAR.100...is.your.family.sharing.the.shelter.with.another.family.",
"VAR.101...does.your.family.have.documentation.that.shows.your.address.in.myanmar.",
"VAR.103...what.are.the.top.three.main.safety.and.security.concerns.for.your.family.",
"VAR.251...if.you.wanted.to.complain.or.raise.a.problem.related.to.assistance.in.the.camps..where.would.you.go..i.don.t.know") 

critical.values <- c(1, "No", "Yes", "No", "None/no others", 1)# the value the variable should take to be counted (in the same order as the variables)
weights <- c(0.5, 1, 0.3, 1, -1, 1) #the weights in the final indicator associated with taking on that value
################# 

################# 
### Execute the compose_indicator function and bind the resulting vector to the data
composite.indicator <- compose_indicator(data, parameters, critical.values, weights)
data <- cbind(data, composite.indicator)

#################
### Visualise results
data$composite.indicator %>% table
hist(data$composite.indicator, breaks = 12)

