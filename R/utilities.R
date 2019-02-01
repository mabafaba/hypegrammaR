.write_to_log<-function(message){

}

logmessage<-function(message){
  message(message)
  .write_to_log(message)
}
