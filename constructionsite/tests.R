
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
      #.install_reachR(T, branch = "develop")
      ####################
      
      source("./scripts/dependencies.R")
      source("./scripts/analyse_indicator.R")
      source("./scripts/map_scales.R")
      source("./scripts/stats.R")
      source("./scripts/plots.R")
      library(reachR)
      #install.packages("bit64")
      library(bit64)
      library(data.table)
      require(survey)
      options(survey.lonely.psu = "average")

####################
# LOAD FILES
####################

    # ALWAYS (or never?) use the reachR load functions. otherwise nothing matches anymore because we harmonise colnames internally
    data<- load_data(file = "./data/kri_winter.csv")
    data %>% glimpse

    populations<-load_samplingframe("./data/kri_winter_weights.csv",
                                    sampling.frame.population.column="population",
                                    sampling.frame.stratum.column = "group",
                                    data.stratum.column = "group", return.stratum.populations = T)
    
    questionnaire <- load_questionnaire(data = "./data/kri_winter.csv", 
                                        questions.file = "./data/questions_kri_winter.csv", 
                                        choices.file = "./data/choices_kri_winter.csv", 
                                        choices.label.column.to.use = "english")

    # percent of questions successfully matched:
    (length(questionnaire$questions$name %>% hasdata)/length(questionnaire$questions$name)*100) %>% round %>% paste0("% questions matched") %>% cat
    if("weights" %in% names(data)){stop("'weights' is not allowed as a column name (will be calculated from the sampling frame)")}

####################
# PARAMETERS
####################

  data.dependent.var = "modality"
  independent.var = "idp.ref"
  hypothesis.type="difference_in_groups"
  

  ####################
# test what becomes analyse_indicator() later:
####################
  
  # analyse_indicator<-function(data, dependent.var, independent.var = NULL, hypothesis.type, do.for.each.unique.value.in.var = NULL){
    data <- data[!(data$modality %in% c("NA", "N/A")),] 

    if(nrow(data)==0){stop('dependent var is all NA')}
    data$group %>% table
    data <- data[!(data$group == "#N/A"),]

    # select methods
    variable_weights <- reachR:::weights_of(data)
   
    design <- svydesign(ids =~1,
                        strata = data$group,
                        weights = variable_weights %>% as.vector,
                        data = data)

 
      # data$percent.change <- as.numeric(sub(",", ".", as.character(data$percent.change)))
    # data$percent.change
    # svymean(data$usd.percentage.change, design, na.rm = T)
    # svyttest(data$'independent.var')~get(data.dependent.var), design)
    # 
    # 
    # svymean(data$survey.benef.perception.process.described, design)
    # 
    # undebug(svymean)
    # svymean(~ as.numeric(age.speaker), design)
    test1 <- svychisq (~modality + idp.ref, design)
    ftable(test1$observed)
    
    testresults<-do.a.chi.sq(dependent.var = data.dependent.var, independent.var = independent.var, design)
    testresults
    
    test_name <- testresults$test.parameters[[3]]
    p_value <- testresults$test.results[[2]]
  
    chart <- reach_style_barchart(group = testresults$names, 
                                  percent = testresults$numbers, 
                                  error_min = testresults$min, 
                                  error_max =  testresults$max)
    
    chart + geom_text(aes(x =4, 
                          y = 2,
                          label= paste0("To determine ", hypothesis.type, "\n", test_name, "\n"
                                        ," returned a p value of ", round(p_value,6))),
                size=3,
                family="Arial Narrow",
                col='#000000',
                hjust=0,
                vjust=0.5)

    tb<-svytable(~idp.ref + modality, design) 
    prop.table(tb, 1)
    
    svyciprop(testresults[[numbers]] design)
    x_sq$observed #gives you the table
    x_sq$statistic #

    

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

  

