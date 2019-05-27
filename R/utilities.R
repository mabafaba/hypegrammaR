.write_to_log<-function(message){

}

logmessage<-function(message){
  message(message)
  .write_to_log(message)
}

is_good_dataframe <- function(data){
  if(!is.data.frame(data)){return(FALSE)}
  if(ncol(data)<1){return(FALSE)}
  if(nrow(data)<1){return(FALSE)}
  if(as.vector(data) %>% is.na %>% all){return(FALSE)}
  return(TRUE)
}
