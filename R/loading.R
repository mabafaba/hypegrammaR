to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}
to_alphanumeric_lowercase_colnames_df <- function(df){
  names(df) <- to_alphanumeric_lowercase(names(df))
  return(df)
}
read.csv.auto.sep<-function (file, stringsAsFactors = F, ...){
  df <- data.table::fread(file, stringsAsFactors = stringsAsFactors, ...) %>%
    as.data.frame
  colnames(df) <- to_alphanumeric_lowercase(colnames(df))
  return(df)
}

#' load asessment data
#'
#' @param file path to a csv file with the assessment data
#'
#' @details the data _must_ be in standard kobo format with xml style headers.
#' @value the data from the csv files as data frame. Column header symbols are changed to lowercase alphanumeric and underscore; everything else is converted to a "."
#' @export
load_data<-function(file){
  data <- read.csv.auto.sep(file, stringsAsFactors = F)
  names(data) <- to_alphanumeric_lowercase(names(data))
  return(data)
}




#' Create weighting from a sampling frame
#'
#' @inheritParams surveyweights::weighting_fun_from_samplingframe
#' @details Create a 'weighter' function from a sampling frame data frame. Uses surveyweights::weighting_fun_from_samplingframe()
#' @export
load_samplingframe<-function(file){
  samplingframe<-data.table::fread(file = file)
}


#' Create questionnaire from csv files
#'
#'
#' @details This enables new functions associated with the questionnaire. It uses load_questionnaire() from the koboquest package.
#' @inheritParams koboquest::load_questionnaire
#' @export
load_questionnaire<-function(...){
  questionnaire<-koboquest::load_questionnaire(...)
}





