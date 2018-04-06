
####################
SETUP
####################
# make sure this works again when you start the next day (don't assume commands from other files have been run), e.g this one
# set wd to this script's folder
rm(list = ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
getwd()

####################
# run once
####################
# install.packages("data.table")
# .install_reachR(T)
####################

source("./dependencies.R")
source("./analyse_indicator.R")
source("./map_scales.R")
source("./stats.R")
source("./plots.R")
library(reachR)
require(survey)

####################
# LOAD FILES
####################

# ALWAYS (or NEVER) use the reachR load functions. otherwise nothing matches anymore because we harmonise colnames internally
data<- load_data(file = "./reach_som_protection_assessment_hh_cleaneddata_feb_2018_2.csv")
data %>% glimpse
populations<-load_samplingframe("./sf.csv",
                                sampling.frame.population.column="Population",
                                sampling.frame.stratum.column = "Camp_",
                                data.stratum.column = "overview/camp_name", return.stratum.populations = T)

questionnaire <- load_questionnaire(data, questions.file = "./questionscomma2.csv", choices.file = "./choices2.csv", choices.label.column.to.use = "english")

# percent of questions successfully matched:
(length(questionnaire$questions$name %>% hasdata)/length(questionnaire$questions$name)*100) %>% round %>% paste0("% questions matched") %>% cat


####################
# PARAMETERS
####################

data.dependent.var = "how.many.children.in.the.household.work."
independent.var = "what.is.the.gender.of.the.respondent"
hypothesis.type="direct_reporting"




find.data.types(data.dependent.var = data.dependent.var,independent.var = independent.var)


independent.var <-"Is this an IDP settlement?" %>% (reachR:::to_alphanumeric_lowercase)

# remind me to implemement this
strata_of<-function(data){
  return(data[,"what.region.is.the.assessment.being.conducted.in."])
}



variable.types <- "numeric"
choose.test(hypothesis.type = hypothesis.type, variable.types = variable.types)

#Perform test
# let's pretend the choose.test function decided a chi squared test was appropriate because we are comparing difference in groups
# for a categorical variable


independent.var <- "what.is.the.gender.of.the.respondent."











weight <- read.csv2("./weights.csv", header = T,stringsAsFactors = F )
weight


 rm(sampling.frame.stratum.column)
weight %>% lapply(typeof)
# weight <- weight[,c(1,2)]

strata <- "Camp_"
weightz<- na.omit(weight$Weight...National) 
strata1 <- na.omit(weight$Camp_)
# names(weight) <- c("sampling.frame.stratum.column", "weights")

weight[[weightz]] %>% as.numeric -> weights
weight[[sampling.frame.stratum.column]] %>% typeof



new_data <- read.csv2("./IRQCCCM.csv", header = T)

W <- as.vector(weight$weights)
rm(weights)

design <- svydesign(ids =~0, probs=NULL, strata = strata1, data = new_data, weights = weights)
survey:::svydesign

sf_raw<-read.csv2("./weights.csv",stringsAsFactors = F, header = T)
# create unique strata names from sampling frame
unique_strata <- sf_raw[, sampling.frame.stratum.column]

(unique_strata %>% hasdata %>% table)
