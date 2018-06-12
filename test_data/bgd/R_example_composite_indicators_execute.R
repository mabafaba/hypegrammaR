rm(list=ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")
source("./R_example_composite_indicators_functions.R")

data<-load_assessment_bgd()

#make a list of variables entering the composite indicator with their relative weights 
parameters <- c("VAR.251...if.you.wanted.to.complain.or.raise.a.problem.related.to.assistance.in.the.camps..where.would.you.go..i.don.t.know",
"VAR.58...what.is.are.the.problem.s..related.to.the.latrine.if.any..it.is.not.safe..no.door..no.lock..etc..",
"VAR.99...does.the.shelter.door.have.a.lock.for.the.shelter.to.be.locked.from.the.inside.",
"VAR.100...is.your.family.sharing.the.shelter.with.another.family.",
"VAR.101...does.your.family.have.documentation.that.shows.your.address.in.myanmar.",
"VAR.103...what.are.the.top.three.main.safety.and.security.concerns.for.your.family.") 

#Define what should be counted for each variable: the value the variable should take to be counted and 
#the weights in the final indicator associated with taking on that value
critical.values <- c(1, 1, "No", "Yes", "No", "None/no others")
weights <- c(0.5, 0.7, 1, 0.5, 1, -1)


#Execute
composite.indicator <- compose_indicator(parameters, critical.values, weights)
data <- cbind(data, composite.indicator)

