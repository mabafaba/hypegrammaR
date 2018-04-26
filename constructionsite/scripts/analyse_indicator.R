#Wrapper function that takes the variables as inputs, decides which case they fall under and runs appropriate tests
#skeleton
analyse_indicator<-function(data, dependent.var, independent.var = NULL, hypothesis.type, design, do.for.each.unique.value.in.var = NULL){
    
        if(nrow(data)==0){stop('dependent var is all NA')}
    
        summarise.result<- choose.summary(hypothesis.type, data, dependent.var, independent.var)
        test.hypothesis <- choose.test(hypothesis.type, data, dependent.var, independent.var)
        
        summary.result  <- summarise.result(dependent.var,independent.var, design)
        
        hypothesis.test.results<- test.hypothesis(dependent.var,independent.var, design)
        
        visualisation.of.results <- visualise.results(summary.result, hypothesis.test.results)
          
        return(results)}
#skeleton over
  
#would you like to test you function?  
analyse_indicator(data, dependent.var, independent.var, hypothesis.type, design)  

        
        # select methods
        variable_weights <- reachR:::weights_of(data)
        
        design <- svydesign(ids =~1,
                            strata = strata_of(data),
                            weights = variable_weights %>% as.vector,
                            data = data)
        
        # Function that chooses which case your lovely combination of variables falls
        # if it throws an error at the variable_type level, consider changing it to the right type
        # data$dependen.var <- as.numeric(sub(",", ".", as.character(data$dependent.var)))
        choose.summary <- function(hypothesis.type = hypothesis.type,
                                 data = data,
                                 dependent.var = dependent.var,
                                 independent.var = independent.var,
                                 paired = NULL) {
          variable.type <- paste0(reachR:::variable_type(dependent.var), "_", reachR:::variable_type(independent.var))
          typestring <- paste(c("TYPE",hypothesis.type,variable.type, paired), collapse = "_")
          TYPE_difference_in_groups_categorical_categorical <- hypothesis_test_chisquare
          return(get(typestring)) }

        # To be combined with the choose.summary function above so 1) chooses the case 2) determines the relevant
        # summary statistics and tests and visual representations for this case
        choose.test <- function(hypothesis.type = hypothesis.type,
                                data = data,
                                dependent.var = dependent.var,
                                independent.var = independent.var,
                                paired = NULL) {
          variable.type <- paste0(reachR:::variable_type(dependent.var), "_", reachR:::variable_type(independent.var))
          typestring <- paste(c("TYPE",hypothesis.type,variable.type, paired), collapse = "_")
          TYPE_difference_in_groups_categorical_categorical <- hypothesis_test_chisquare
          #TYPE_group_difference_numeric_categorical <- hypothesis_test_difference_in_means
          #TYPE_limit_numeric <- hypothesis_test_one_sample_z_num
          #TYPE_limit_categorical <- hypothesis_test_one_sample_z_cat
          return(get(typestring)) }
        
        visualise.results <- function()
