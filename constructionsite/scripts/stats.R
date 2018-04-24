do.a.chi.sq <- function(independent.var = independent.var,
                        dependent.var = data.dependent.var,
                        design,
                        na.rm = TRUE){
  formula_string<-paste0("~",independent.var, "+", dependent.var)
  chisq <- svychisq(formula(formula_string), design, na.rm = TRUE)
  table <- svytable(formula(formula_string), design)
  formula_err <- paste0("~", dependent.var, sep = "") 
  by <- paste0(" ~", independent.var, sep = "")
  error_bars <- svyby(formula(formula_err), formula(by), design, na.rm = T, svymean)
  results<-list()
  results$test.results <- c(chisq$statistic, chisq$p.value)
  results$test.parameters <- c(chisq$parameter, chisq$method)
  names_df <- sapply(rownames(chisq$observed), paste0, colnames(chisq$observed))
  results$names <- c(names_df[,1], names_df[,2])
  results$numbers <- as.numeric(c(prop.table(table, 1)[1,], prop.table(chisq$observed, 1)[2,]))
  results$se <- as.numeric(c(error_bars[,grep("se.", names(error_bars))][1,], error_bars[,grep("se.", names(error_bars))][2,]))
  results$min <- results$numbers - results$se
  results$max <- results$numbers + results$se
  return(results)
}

hack_chi_sq <- function(data.dependent.var, independent.var, design, data){
  result <- svychisq(~ data$idp.ref + data$modality, design = design, data)
  return(result)
}

hack_chi_sq(data.dependent.var, independent.var, design, data)

rm(statistic)
undebug(svychisq)

hypothesis_test_chisquare(dependentvar = data.dependent.var, independent = independent.var, design = design, data = 
                            data)

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