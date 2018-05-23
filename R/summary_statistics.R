percent_with_confints <- function(independent.var = independent.var,
                                  dependent.var = data.dependent.var,
                                  design,
                                  na.rm = TRUE){
  formula_string<-paste0("~",independent.var, "+", dependent.var)
  f.table <- svytable(formula(formula_string), design)
  
  formula_err <- paste0("~", dependent.var, sep = "") 
  by <- paste0(" ~", independent.var, sep = "")
  error_bars <- svyby(formula(formula_err), formula(by), design, na.rm = T, svymean)
  names_df <- sapply(rownames(f.table), paste0, colnames(f.table))
  results<-list()
  results$names <- c(names_df[,1], names_df[,2])
  results$numbers <- as.numeric(c(prop.table(f.table, 1)[1,], prop.table(f.table, 1)[2,]))
  results$se <- as.numeric(c(error_bars[,grep("se.", names(error_bars))][1,], error_bars[,grep("se.", names(error_bars))][2,]))
  results$min <- results$numbers - results$se
  results$max <- results$numbers + results$se
  return(results)
}



confidence_intervals_num <- function(dependent.var, 
                                     independent.var = NULL,
                                     design, 
                                     data = data){
  formula_string<-paste0("~as.numeric(", dependent.var, ")")
  summary <- svymean(formula(formula_string), design, na.rm = T)
  confints <- confint(summary, level = 0.95)
  results<-list()
  results$names <- dependent.var
  results$numbers <- summary
  results$min <- confints[,1]
  results$max <- confints[,2]
  return(results)
 }
 
