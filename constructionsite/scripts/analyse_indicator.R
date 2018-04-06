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

}