


analyse_indicator<-function(data,
                            dependent.var,
                            independent.var = NULL,
                            hypothesis.type,
                            design,
                            do.for.each.unique.value.in.var = NULL){



        # sanitise input
            data <- data[!is.na(data[,dependent.var]),]
            if(nrow(data)==0){stop('provided data has no rows where dependent.var is not NA')}
            if(all(is.na(data[,dependent.var]))){stop(paste('variable', dependent.var, 'can\'t be all NA'))}

  
        # map from input to analysis case:
        case<- map_to_case(hypothesis.type = hypothesis.type,
                                   data = data,
                                   dependent.var = dependent.var,
                                   independent.var = independent.var,
                                   paired = NULL)
        
  
        # map from case to appropriate summary statistic, hypothesis test and visualisation:
            summarise.result<- map_to_summary_statistic(case)
            test.hypothesis <- map_to_hypothesis_test(case)
            visualisation <- map_to_visualisation(case)
            
            
            
        # apply the ummary statistic, hypothesis test to the given data and survey design:  
            summary.result  <- summarise.result(dependent.var,independent.var, design)
        # do hypothesis test:
            hypothesis.test.result<- test.hypothesis(dependent.var,independent.var, design)
            
        # add results to the visualisation:
            # visualisation<-visualisation+ggplot()...
        return(list(
                    summary.result,
                    hypothesis.test.result,
                    visualisation
              ))
        
    }
  


