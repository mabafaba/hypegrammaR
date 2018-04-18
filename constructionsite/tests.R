
####################
# SETUP
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
      
      source("./scripts/dependencies.R")
      source("./scripts/analyse_indicator.R")
      source("./scripts/map_scales.R")
      source("./scripts/stats.R")
      source("./scripts/plots.R")
      library(reachR)
      require(survey)
      options(survey.lonely.psu = "average")

####################
# LOAD FILES
####################

    # ALWAYS (or never?) use the reachR load functions. otherwise nothing matches anymore because we harmonise colnames internally
    data<- load_data(file = "./data/data_kri.csv")
    data %>% glimpse

    populations<-load_samplingframe("./data/weighting_irq_kri.csv",
                                    sampling.frame.population.column="population.size",
                                    sampling.frame.stratum.column = "stratum.name",
                                    data.stratum.column = "stratum", return.stratum.populations = T)
    
    questionnaire <- load_questionnaire(data, questions.file = "./data/questionscomma2.csv", choices.file = "./choices2.csv", choices.label.column.to.use = "english")
    
    # percent of questions successfully matched:
    (length(questionnaire$questions$name %>% hasdata)/length(questionnaire$questions$name)*100) %>% round %>% paste0("% questions matched") %>% cat


####################
# PARAMETERS
####################

  data.dependent.var = "survey.benef.perception.process.described"
  independent.var = "idp.ref"
  hypothesis.type="direct_reporting"
  
####################
# test what becomes analyse_indicator() later:
####################
  
  # analyse_indicator<-function(data, dependent.var, independent.var = NULL, hypothesis.type, do.for.each.unique.value.in.var = NULL){
    data <- data[!(data$survey.benef.perception.process.described %in% c("NA", "N/A")),] 
    data$survey.benef.perception.process.described %>% table
    
    data$idp.ref[data$idp.ref == "host_community"] <- "idp"

    if(nrow(data)==0){stop('dependent var is all NA')}
    data$stratum %>% table
    data <- data[!(data$stratum == ""),]

    # select methods
    variable_weights <- reachR:::weights_of(data)
    
    design <- svydesign(ids =~1,
                        strata = data$stratum,
                        weights = variable_weights %>% as.vector,
                        data = data)

    data$percent.change <- as.numeric(sub(",", ".", as.character(data$percent.change)))
    data$percent.change
    svymean(data$usd.percentage.change, design, na.rm = T)
    svyttest(percent.change~idp.ref, design)
    

    svymean(data$survey.benef.perception.process.described, design)
    
    undebug(svymean)
    svymean(~survey.benef.perception.process.described, design)
    
    x_sq <- svychisq(~survey.benef.perception.process.described+idp.ref, design, statistic = "Wald")

    
  design$variables[, as.character(cols)]
    
    variable.types<-find.data.types(dependent.var, independent.var)
         # i think for data column names we should do:   [SOURCE].[WHAT].column: e.g. data.dependent.var.column, samplingframe.stratum.column, etc.
          # what if there's more than 1 independent var?
          # WHAT HAPPENS IF DATA TYPE CANT BE DETERMINED? - IF FOR ONE VARIABLE BOTH GO TO FALSE? -> currently it returns as if only one var was supplied  that's no good.
          # it should guess the data type from the data, and throw a warning if: missmatch with questionnaire or if: not found in questionnaire.
          # this is a bit dangerous if people have numbers with text ("unknown", "no data" or something) in some values  etc..
          # maybe we have to supply this is a parameter...

    stat.test <- choose.test(hypothesis.type = hypothesis.type, variable.types = variable.type)
    
    stat.test(dependentvar = dependent.var,independent = independent.var,design = )
    # results need to be stored in a variable
    
    
    
  
    #giving some summary statistics
    # design<-make.svydesign()
    # mean <- svymean(data[[dependentvar]], design, na.rm = T)
    stat.test.result <- stat.test(....,.....)
    
  # }
  
  
  

