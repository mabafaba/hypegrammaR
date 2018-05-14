hypothesis_test_chisquared <- function(independent.var = independent.var,
                                  dependent.var = data.dependent.var,
                                  design){
  formula_string<-paste0("~",independent.var, "+", dependent.var)
  chisq <- svychisq (formula(formula_string), design, na.rm = TRUE)
  results$test.results <- c(chisq$statistic, chisq$p.value)
  results$test.parameters <- c(chisq$parameter, chisq$method)
  results$hypothesis_test<-"chi-squared test of independence"
  return(results)
}
