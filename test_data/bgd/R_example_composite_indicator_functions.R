# Internal function that recodes each element (variable) of the composite indicator into a vector with value = weight if the critical value is met and 0 otherwise
recode.elements <- function(vector, critical.value, weight){
  recoded.v <- vector %in% critical.value ## recodes the vector into T/F depending on whether the value matches the critical value. 
  recoded.v[recoded.v == TRUE] <- weight
  recoded.v[recoded.v == FALSE] <- 0
  return(recoded.v)
}

# Builds the composite indicator by summing the recoded individual elements. 
compose_indicator <- function(data, parameter, critical.value, weight){
  data_ci <- data[match(parameter, names(data))] #take a subset of the data with only the columns needed for the composite indicator
  value_per_indicator <- mapply(recode.elements, data_ci, critical.value, weight) %>% as.data.frame
  indicator <- rowSums(value_per_indicator) / max(rowSums(value_per_indicator)) #Divides by the maximum to get a composite indicator >1
  return(indicator)}

