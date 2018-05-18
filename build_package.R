



build<-function(package.name,first.time=F){
  all_objects<-ls()
    rm(list=  all_objects[!(all_objects %in% c("package.name","first.time"))])
  getwd()
  # try(detach(paste("package",package.name,sep=":")))
  this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
  setwd(this_script_path)

  
  require("roxygen2")
    print(first.time)
  if(first.time){
    setwd("..")
    create(package.name)
    setwd(package.name)
  }
  
  package_source_files<-paste0("./R/", list.files("./R/"))
  sapply(package_source_files,source)
  require("devtools")
  roxygenize(clean=T)
}


build("hypegrammaR",first.time = F)
