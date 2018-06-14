# changes all occurences of a specific value in a vector to a specified value; change all other values to a different value

recode_binary <- function(x, from, to, otherwise = 0){
  condition_met <- x %in% from # check which value matches the critical value
  x_recoded<-rep(NA,length(x))
  x_recoded[condition_met] <- to  # recode to "to" value where condition met
  x_recoded[!condition_met & !is.na(x)] <- otherwise  # recode to "otherwise" value otherwise (unless where x was NA):
  return(x_recoded)
}

# Builds the composite indicator by summing the recoded individual elements.
compose_indicator_binarised_weighted <- function(data, variables, critical.values, weights){

  data_subset <- data[, variables] #take a subset of the data, with only the columns needed for the composite indicator
  value_per_indicator <- mapply(recode_binary, data_subset, critical.values, weights) %>% as.data.frame
  indicator <- rowSums(value_per_indicator,na.rm = F) / max(rowSums(value_per_indicator),na.rm = T) #Divides by the maximum to get a composite indicator <1
  return(indicator)

  }
