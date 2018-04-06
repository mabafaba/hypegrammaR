hypothesis_test_chisquare <- function(dependentvar, independent, design, data = data) {
  Chisq <- svychisq(~ dependentvar + independent, design, na.rm = TRUE)
  # Groups <-
  namesA <- unique(dependentvar)
  namesB <- unique(independent)
  b <- ftable(a, rownames = namesA, namesB)
  return(b)
}



hypothesis_test_one_sample_z_num <- function(data.dependent.var, crit, design, data = data) {
  svyttest(data[[dependentvar]]~data[[independent.var]], design = design, family = quasibinomial())
}

confidence_intervals_num <- function(dependentvar, design, data = data){
  summary <- svymean(data[[dependentvar]], design, na.rm = T)
  confint(svymean(data[[dependentvar]] + data[[dependentvar]], design, na.rm = T), level = 0.95)
}



reformat_svy_results<-function(x){
  return(x)
}