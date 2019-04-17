hypothesis_test_chisquared_select_one <- function(dependent.var,
                                                  independent.var,
                                                  design){

  # sanitise design
  sanitised<-datasanitation_design(design,dependent.var,independent.var,
                                   datasanitation_hypothesistest_chisq)
  if(!sanitised$success){return(hypothesis_test_empty(dependent.var,independent.var,message=sanitised$message))}
  # update design object from sanitation
  design<-sanitised$design


  formula_string<-paste0("~",independent.var, "+", dependent.var)

  # drop empty choices from levels (to avoid many empty cells, potentially breaking the chisquared test)
  if(is.factor(design$variables[[independent.var]])){design$variables[[independent.var]]<-droplevels(design$variables[[independent.var]])}
  if(is.factor(design$variables[[dependent.var]])){design$variables[[dependent.var]]<-droplevels(design$variables[[dependent.var]])}

  tryCatch(
    {chisq <- svychisq (formula(formula_string), design, na.rm = TRUE)},
    error=function(e){
      # .write_to_log(paste0("FAILED: Chi squared test.  Error:\n",e,"\n"))
      # .write_to_log(paste0("independent.var:",independent.var,"- dependent.var:",dependent.var,"\n raw frequency table:"))
      # .write_to_log(kable(table(design$variables[,c(dependent.var,independent.var)])))
      return(hypothesis_test_empty(message=e$message))}

  )
  if(exists("chisq")){
    results<-list()
    results$result <- list(F=chisq$statistic, p.value=chisq$p.value %>% unname)
    results$parameters <- chisq$parameter %>% as.list
    results$name<-chisq$method
    return(results)}
}

######## ONE SAMPLE Z tEST
# hypothesis_test_one_sample_z_num <- function(data.dependent.var, crit, design, data = data) {
# doesn't seem right.. parameter 'crit' not used.
#   svyttest(data[[dependentvar]]~data[[independent.var]], design = design, family = quasibinomial())
# }


hypothesis_test_empty <- function(dependent.var = NULL,
                                  independent.var = NULL,
                                  design = NULL,
                                  message="No hypothesis test",...){
  results<-list()
  results$result <- c()
  results$parameters <- c()
  results$name<-message
  return(results)
}


hypothesis_test_t_two_sample <- function(dependent.var,
                                         independent.var,
                                         design){


  as.numeric_factors_from_names<-function(x){
    if(is.factor((x))){x<-as.character(x)}
    as.numeric(x)
  }

  design$variables[[dependent.var]] <- as.numeric_factors_from_names(design$variables[[dependent.var]])
  if(is.factor(design$variables[[independent.var]])){
    design$variables[[independent.var]]<-droplevels(design$variables[[independent.var]])
  }



  independent_more_than_1 <- length(unique(design$variables[[independent.var]])) > 1
  if(!independent_more_than_1){
    results <- list()}else{
      formula_string<-paste0(dependent.var, "~", independent.var)
      ttest <- svyttest(formula(formula_string), design, na.rm = TRUE)
      results<-list()
      results$result <- list(t=unname(ttest$statistic), p.value = ttest$p.value %>% unname)
      results$parameters <- as.list(ttest$parameter)
      results$name<-"two sample ttest on difference in means (two sided)"}
  return(results)

  ttest$statistic

}


hypothesis_test_t_one_sample <- function(dependent.var,
                                         independent.var = NULL,
                                         limit,
                                         design){

  as.numeric_factors_from_names<-function(x){
    if(is.factor((x))){x<-as.character(x)}
    as.numeric(x)
  }

  limit <- as.numeric(limit)
### how to make this function one sided

  design$variables[[dependent.var]]  <- as.numeric_factors_from_names(design$variables[[dependent.var]])
  design$variables[[dependent.var]] <- design$variables[[dependent.var]] - limit

  sanitised<-datasanitation_design(design,dependent.var,independent.var,
                                   datasanitation_hypothesistest_limit)

      formula_string<-paste0(dependent.var, "~", 0)
      ttest <- svyttest(formula(formula_string), design, na.rm = TRUE, alternative = "two.sided")
      results<-list()
      results$result <- list(t=unname(ttest$statistic), p.value = ttest$p.value %>% unname)
      results$parameters <- as.list(ttest$parameter)
      results$name<- paste0(c("one sample ttest on difference from", limit, "limit value (one sided)"))
  return(results)

  ttest$statistic

}

model_logistic_regression <- function(dependent.var,
                                                independent.var,
                                                design){

  dependent_more_than_1 <- length(unique(design$variables[[dependent.var]])) > 1
  dependent_binary <- length(!is.na(as.logical(design$variables[[dependent.var]]))) == length(!is.na(design$variables[[dependent.var]]))

  if(!(dependent_more_than_1 & dependent_binary)){
    sanitised <-sanitise_data(design$variables,dependent.var,independent.var,case = case)
    results <- list()}else{
      formula_string <- paste0(dependent.var,"~", independent.var, sep = "")
      test <- svyglm(as.formula(formula_string), design, family=quasibinomial)
      summary <- summary(test)
      results <- list()
      ## this needs to macth the format for the other ones
      ## coefficient to be interpreted suuuper carefully
      results$result <- data.frame(summary$coefficients)
    }
  return$results
}


model_linear_regression <- function(dependent.var,
                                                independent.var,
                                                design){

  dependent_more_than_10 <- length(unique(design$variables[[dependent.var]])) > 10
  independent_more_than_1 <- length(unique(design$variables[[independent.var]])) > 1

  if(!(dependent_more_than_10 & independent_more_than_1)){
    sanitised <-sanitise_data(design$variables,dependent.var,independent.var,case = case)
    results <- list()}else{
      formula_string <- paste0(dependent.var,"~", independent.var, sep = "")
      test <- svyglm(as.formula(formula_string), design, family=stats::gaussian())
      summary <- summary(test)
      results <- list()
      ## this needs to macth the format for the other ones
      ## where can I get an R squared to add to this ??
      results$result <- data.frame(summary$coefficients)
    }
  return$results
}

hypothesis_test_z <- function(dependent.var,
                              independent.var,
                              design){
  # .....


  results<-list()
  results$result <- c()
  results$parameters <- c()
  results$name<-"Z test (not implemented)"
  return(results)
}




hypothesis_test_chisquared_select_multiple <- function(dependent.var,
                                                       dependent.var.sm.cols,
                                                       independent.var,
                                                       design){

  # sanitise design
  for(x in dependent.var.sm.cols){
  dependent.var <- names(design$variables)[x]
  sanitised<-datasanitation_design(design,dependent.var,independent.var,
                                   datasanitation_hypothesistest_chisq)
  if(!sanitised$success){return(hypothesis_test_empty(dependent.var,independent.var,message=sanitised$message))}

  design<-sanitised$design}


  multiple_dependents<-names(design$variables)[dependent.var.sm.cols]

  multiple_results<-lapply(multiple_dependents,function(dependent.var){
    formula_string<-paste0("~",independent.var, "+", dependent.var)
    chisq<-tryCatch(
      {svychisq (formula(formula_string), design, na.rm = TRUE)
      },
      error=function(e){

        return(NULL)}

    )
  })
  `%pull%`<-function(list,item){lapply(list,function(x){x[[item]]})}
  successful<-sapply(multiple_results,function(x){!is.null(x)})
  p.values<-(multiple_results %pull% "p.value") %>% unlist %>% .[successful]
  choicesnames<-multiple_dependents %>% gsub(paste0(dependent.var,"."),"",.) %>% .[successful]
  names(p.values)<-choicesnames %>% unlist
  fstats<-multiple_results %pull% "statistic" %>% unlist %>% .[successful]
  names(fstats)<-choicesnames
  method<-multiple_results %pull% "method" %>% unlist %>% .[successful]
  names(method)<-choicesnames %>% unlist
  results<-list()
  results$result <- list(F=fstats, p.value=p.values)
  results$parameters <- multiple_results %pull% "parameter" %>% .[successful] %>% do.call(rbind,.)

  results$name<-method
  results <- results %>% data.frame %>% (function(x){names(x)<-gsub("^result\\.","",names(x));x})
  return(results)
}
