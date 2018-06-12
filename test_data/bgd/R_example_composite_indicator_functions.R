# Internal function that calculates the weight for each element of the composite indicator
composite_indicator_elements <- function(x, y, z){
  sum <- data[x][[1]] %in% y ## strange that data[x] transforms it into a list but a priori taking the first element will always get to the data in vector form
  sum[sum == TRUE] <- z
  sum[sum == FALSE] <- 0
  return(sum)
}

# sum the individual elements to make the composite indicator
# input sanitation should check that parameter, critical value and weight all have the same length. could eventually also be loaded as a df?
compose_indicator <- function(parameter, critical.value, weight){
  value_per_indicator <- mapply(composite.indicator.elements, parameter, critical.value, weight) %>% as.data.frame
  indicator <- rowSums(value_per_indicator) / max(rowSums(value_per_indicator))
  return(indicator)}
