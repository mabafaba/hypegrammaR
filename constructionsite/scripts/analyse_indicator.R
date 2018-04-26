
  analyse_indicator<-function(data, dependent.var, independent.var = NULL, hypothesis.type, do.for.each.unique.value.in.var = NULL){
        results <- list()
        data <- data[!is.na(dependent.var),]
        data <- data[!(dependent.var %in% c("NA", "N/A")),] 
        if(nrow(data)==0){stop('dependent var is all NA')
        variable.type <- find.data.types(dependent.var, independent.var)
        choo
        return(results)}
        
        # select methods
        variable_weights <- reachR:::weights_of(data)
        
        design <- svydesign(ids =~1,
                            strata = strata_of(data),
                            weights = variable_weights %>% as.vector,
                            data = data)
        
        
        variable.type.s<-find.data.types(dependent.var, independent.var)
        # what if there's more than 1 independent var?
        # doesn't work currently. When we build it out, I suppose it would be indep 1,2,...
        
        
        # WHAT HAPPENS IF DATA TYPE CANT BE DETERMINED? - IF FOR ONE VARIABLE BOTH GO TO FALSE? -> currently it returns as if only 
        # one var was supplied  that's no good.
        # it should guess the data type from the data, and throw a warning if: missmatch with questionnaire or if: not found in questionnaire.
        # this is a bit dangerous if people have numbers with text ("unknown", "no data" or something) in some values  etc..
        # maybe we have to supply this as a parameter...
        
        #I've added ... to allow for future arguments like 2 independent vars

        
        stat.test <- choose.test(hypothesis.type = hypothesis.type, variable.types = variable.type.s)
        
        choose.test <- function(hypothesis.type = hypothesis.type,
                                 data = data,
                                 dependent.var = dependent.var,
                                 independent.var = independent.var){
          variable_type(dependent.var, independent.var)
          typestring <- paste(c("TYPE",hypothesis.type,variable.types, paired), collapse = "_")
          TYPE_group_difference_categorical_categorical <- hypothesis_test_chisquare
          TYPE_group_difference_numeric_categorical <- hypothesis_test_difference_in_means
          TYPE_limit_numeric <- hypothesis_test_one_sample_z_num
          TYPE_limit_categorical <- hypothesis_test_one_sample_z_cat
          return(get(typestring)) }
        
        hypothesis_test_chisquare <- function(dependent.var, independent.var, design){
          do.a.chi.sq(independent.var = independent.var, dependent.var = dependent.var, design = design)
          table.of.data(independent.var = independent.var, dependent.var = dependent.var, design = design)
        }
        
        stat.test(dependentvar = dependent.var,independent = independent.var,design = )
        # results need to be stored in a variable
        
        
        
        
        #giving some summary statistics
        # design<-make.svydesign()
        # mean <- svymean(data[[dependentvar]], design, na.rm = T)
        stat.test.result <- stat.test(....,.....)

}