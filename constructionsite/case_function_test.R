
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

# load_clusterframe <- function(cluster.sampling.frame.file, ...)


#choose test
#still need to add sampling strategy = in here, but for now this package deals with simple random, stratified and cluster in the same way
#and the others not at all

variable.types <- "numeric"
choose.test(hypothesis.type = hypothesis.type, variable.types = variable.types)

#Perform test
# let's pretend the choose.test function decided a chi squared test was appropriate because we are comparing difference in groups
# for a categorical variable

  
independent.var <- "what.is.the.gender.of.the.respondent."
think this function should just take any number of names (either as a vector or with ...). Then get the type for each and concat them together.
      # considering that some are dependent and some are independent is not a thing "find.data.types" should have to worry about. it should find data types and that's it.
      # CONSISTENT PARAMETER NAMES: first starts with data, second not
    


