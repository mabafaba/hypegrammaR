hypothesis_test_chisquared <- function(independent.var = independent.var,
                                  dependent.var = data.dependent.var,
                                  design){
  formula_string<-paste0("~",independent.var, "+", dependent.var)
  ttest <- svychisq (formula(formula_string), design, na.rm = TRUE)
  results<-list()
  results$test.results <- c(chisq$statistic, chisq$p.value)
  results$test.parameters <- c(chisq$parameter, chisq$method)
  results$hypothesis_test<-"chi-squared test of independence"
  return(results)
}


########ONE SAMPLE Z tEST 
# hypothesis_test_one_sample_z_num <- function(data.dependent.var, crit, design, data = data) {
# doesn't seem right.. parameter 'crit' not used.
#   svyttest(data[[dependentvar]]~data[[independent.var]], design = design, family = quasibinomial())
# }


hypothesis_test_empty <- function(independent.var = NULL,
                                       dependent.var = data.dependent.var,
                                       design){
  results<-list()
  results$test.results <- c()
  results$test.parameters <- c()
  results$hypothesis_test<-"No Hypothesis test"
  return(results)
}


hypothesis_test_t_two_sample <- function(independent.var = independent.var,
                                       dependent.var = data.dependent.var,
                                       design){
  formula_string<-paste0(dependent.var, "~", independent.var)
  ttest <- svyttest(formula(formula_string), design, na.rm = TRUE)
  results<-list()
  results$test.results <- c(ttest$statistic, ttest$p.value)
  results$test.parameters <- c(ttest$parameter, ttest$method)
  results$hypothesis_test<-"two sample ttest on difference in means"
  return(results)
}





hypothesis_test_z <- function(independent.var = independent.var,
                               dependent.var = data.dependent.var,
                               design){
  # .....
  
  
  results<-list()
  results$test.results <- c()
  results$test.parameters <- c()
  results$hypothesis_test<-""
  return(results)
}


hypothesis_test_linear_regression <- function(independent.var = independent.var,
                              dependent.var = data.dependent.var,
                              design){

  
  # .....
  
  
  results<-list()
  results$test.results <- c()
  results$test.parameters <- c()
  results$hypothesis_test<-""
  return(results)
}




