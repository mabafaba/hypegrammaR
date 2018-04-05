# make sure this works again when you start the next day (don't assume commands from other files have been run), e.g this one
# set wd to this script's folder
rm(list = ls())
this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
getwd()

####################
# run once
####################
install.packages("data.table")
.install_reachR(T)
####################

source("./dependencies.R")
library(reachR)
require(survey)

#load input
  data<- reachR:::read.csv.auto.sep(file = "./IRQ_CCCM.csv")

#load sampling frame
samplingframe<- reachR:::read.csv.auto.sep(file = "sf.csv")
populations<-load_samplingframe("./sf.csv",
                                sampling.frame.population.column="Population",
                                sampling.frame.stratum.column = "Camp_",
                                data.stratum.column = "overview/camp_name", return.stratum.populations = T)

#load questionnaire
questionnaire <- load_questionnaire(data, questions.file = "./questionscomma2.csv", choices.file = "./choices.csv", choices.label.column.to.use = "english")
undebug(load_questionnaire)

#some sanity checks
reachR:::load_questionnaire
names(read.csv("./questions.csv", stringsAsFactors = F, header= T))
undebug(load_questionnaire)

#checking which question names made it through
questions <- questionnaire[[1]]
questions$name

data.dependent.var = "hh_profile/hhh_age"
independent.var = "settlement"
hypothesis.type="direct_reporting"
#sampling.strategy="stratified random"

#das könnte auch eleganter gehen mit funktion über x und jeder variable in x einen Type dazu machen
find.data.types <- function(data.dependent.var, independent.var = NULL) {
  data.type.dep = c()
  data.type.indep = c()
  if(question_is_categorical(data.dependent.var) == T){
    data.type.dep = "categorical"
  }
  if(question_is_numeric(data.dependent.var) == T){
    data.type.dep = "numeric"
  }
  if(question_is_categorical(independent.var) == T){
    data.type.indep = "categorical"
  }
  if(question_is_numeric(independent.var) == T){
    data.type.indep = "numeric"
  }
  variable.types <- paste(data.type.dep, data.type.indep, sep="_")
  return(variable.types)
}



find.data.types(data.dependent.var)


debug(question_is_numeric)
reachR:::question_is_numeric


#seperate cases 
data$`overview/camp_name`
strata_of<-function(data){
  return(data[,"overview/camp_name"])
}


analyse_indicator<-function(data, dependent.var, independent.var = NULL, hypothesis.type, do.for.each.unique.value.in.var = NULL){
  data <- data[!is.na(dependent.var),]
  if(nrow(data)==0){stop('dependent var is all NA')}

  # select methods
  variable_weights <- reachR:::weights_of(data)
  
  design <- svydesign(ids =~1,
                      strata = strata_of(data),
                      weights = variable_weights %>% as.vector,
                      data = data)

  variable.types<-find.data.types(dependent.var, independent.var)
  
  
  stat.test <- choose.test(hypothesis.type = hypothesis.type, variable.types = variable.type)

  
  stat.test(dependentvar = dependent.var,independent = independent.var,design = )
  
  type_independent <- reachR:::question_is_categorical_internal(dependent.var)


      #giving some summary statistics
        # design<-make.svydesign()
        mean <- svymean(data[[dependentvar]], design, na.rm = T)
        stat.test.result <- stat.test(....,.....)

        }


# load_clusterframe <- function(cluster.sampling.frame.file, ...)
# load_questionnaire()
# load_choices


#choose test
#still need to add sampling strategy = in here, but for now this package deals with simple random, stratified and cluster in the same way
#and the others not at all
choose.test <- function(hypothesis.type, variable.types, crit = NULL, paired = NULL){
  
  typestring <- paste(c("TYPE",hypothesis.type,variable.types, paired), collapse = "_")
  # typestring="TYPE_direct_reporting_simple_random_numeric"
  TYPE_group_difference_categorical_categorical <- hypothesis_test_chisquare
  # TYPE_group_difference_numeric_categorical <- hypothesis_test_difference_in_means
  # TYPE_limit_numeric <- hypothesis_test_one_sample_z_num
  # TYPE_limit_categorical <- hypothesis_test_one_sample_z_cat
  TYPE_direct_reporting_numeric <- confidence_intervals_num
  # TYPE_direct_reporting_categorical <- confidence_intervals_cat
  # TYPE_change_over_time_numeric <- hypothesis_test_difference_in_means #paired or unpaires
  # TYPE_change_over_time_categorical <- hypothesis_test_chisquare
  # TYPE_correlation_numeric_numeric <- hypothesis_test_regression
  # TYPE_correlation_categorical_numeric <- hypothesis_test_logistic_regression #warn: categorical variable must be binary (ensure)
  #

  return(get(typestring))

}

variable.types <- "numeric"
choose.test(hypothesis.type = hypothesis.type, variable.types = variable.types)

#Perform test
# let's pretend the choose.test function decided a chi squared test was appropriate because we are comparing difference in groups
# for a categorical variable
hypothesis_test_chisquare <- function(dependentvar, independent, design, data = data) {
  Chisq <- svychisq(~ dependentvar + independent, design, na.rm = TRUE)
  # Groups <-
  namesA <- unique(dependentvar)
  namesB <- unique(independent)
  b <- ftable(a, rownames = namesA, namesB)
  return(b)
  }
  
hypothesis_test_one_sample_z_num <- function(data.dependent.var, crit, design, data = data) {
  svyttest(data.dependent.var~1, null.value = crit, design = design)
}


confidence_intervals_num <- function(dependentvar, design, data = data){
  summary <- svymean(data[[dependentvar]], design, na.rm = T)
  confint(svymean(data[[dependentvar]] + data[[dependentvar]], design, na.rm = T))
  }


#Spit out results


#Plot
  reach_style_barchart<-function(group,percent,error_min=NULL,error_max=NULL,horizontal=T){
    require('extrafont')
    require('ggplot2')
    require("ggthemes")
    require("dplyr")
    require('grid')

    percent_formats<-function(x,digits=0){return(paste0(round(x*100,digits),"%"))}

    df<-data.frame(group=group,percent=percent,ymin=error_min,ymax=error_max)

    colnames(df)<-c('group','percent','ymin','ymax')

    theplot<-

      ggplot(df)+
      geom_bar(aes(y=1,x=group),
                     stat='identity',
                     fill=reach_style_color_lightgrey()) +
      geom_bar(aes(y=percent,x=group),
                     stat='identity',
                     fill=reach_style_color_red()) +

      geom_errorbar( aes(x=group,
                         y=percent,
                         ymin=ymin,
                         ymax=ymax),
                     stat='identity',
                     width=0) +

      geom_text(aes(x=group, y=(1), label=paste0("  ", round(percent*100),"%"," ",group)),size=4,
                    family="Arial Narrow",
                    col='#000000',
                    hjust=0,
                    vjust=0.5) +

      theme_tufte()+reachR:::reach_theme() +
      theme(text=element_text(family="Arial Narrow"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.margin = unit(x = c(0,0,0,0),'null')) +

      scale_y_continuous(labels = percent_formats,limits=c(0,3))

    if(horizontal){
      theplot<-theplot+coord_flip()}
    return(theplot)
  }

