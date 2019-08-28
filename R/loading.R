

to_alphanumeric_lowercase <- function(x){tolower(gsub("[^a-zA-Z0-9_]", "\\.", x))}


to_alphanumeric_lowercase_colnames_df <- function(df){
  names(df) <- to_alphanumeric_lowercase(names(df))
  return(df)
}

#' loading function with automatic default
#'
#' @param file path to a csv file with the assessment data
#'
#' @details the file is loaded with stringsAsFactors = F and with column names in alphanumeric lowercase
#' @return the data from the csv files as data frame. Column header symbols are changed to lowercase alphanumeric and underscore; everything else is converted to a "."
#' @export
read.csv.auto.sep<-function (file, stringsAsFactors = F, ...){
  df <- data.table::fread(file, stringsAsFactors = stringsAsFactors, ...) %>% as.data.frame
  colnames(df) <- to_alphanumeric_lowercase(colnames(df))
  return(df)
}

#' load asessment data
#'
#' @param file path to a csv file with the assessment data
#'
#' @details the data _must_ be in standard kobo format with xml style headers.
#' @return the data from the csv files as data frame. Column header symbols are changed to lowercase alphanumeric and underscore; everything else is converted to a "."
#' @export
load_data<-function(file){

  assertthat::assert_that(grepl(x = file, pattern = ".csv$"),msg = "file must end with '.csv' (..and actually be a .csv file)")
  data <- read.csv.auto.sep(file, stringsAsFactors = F)
  names(data) <- to_alphanumeric_lowercase(names(data))
  return(data)
}



#' Load a sampling frame from csv
#' @param file the path and name of the sampling frame csv file to load.
#' @details function loads the sampling frame and can be used to make weights ith map_to_weighting()
#' @examples \dontrun{sf <- load_samplingframe("./somefolder/samplingframe.csv")}
#'
#' @export
load_samplingframe<-function(file){

  assertthat::assert_that(assertthat::is.readable(file))
  assertthat::assert_that(grepl(x= file, pattern = ".csv$"),msg = "file must end with '.csv' (..and actually be a .csv file)")
  samplingframe<-read.csv.auto.sep(file, stringsAsFactors = F)
}


#' load_questionnaire
#' @param data data frame containing the data matching the questionnaire to be loaded.
#' @param questions data frame or file name of a csv file containing the kobo form's question sheet
#' @param choices data frame or file name of a csv file containing the kobo form's choices sheet
#' @param choices.label.column.to.use The choices csv file has (sometimes multiple) columns with labels. They are often called "Label::English" or similar. Here you need to provide the _name of the column_ that you want to use for labels (see example!)
#' @return A list containing the original questionnaire questions and choices, the choices matched 1:1 with the data columns, and all functions created by this function relating to the specific questionnaire (they are written to the global space too, but you can use these when using multiple questionnaires in parallel.)
#' @export
#' @examples
#'\dontrun{
#'load_questionnaire(mydata,
#'                   questions.file="koboquestions.csv",
#'                   choices.file="kobochoices.csv",
#'                   choices.label.column.to.use="Label::English")
#'}
#'
load_questionnaire<-function(data,
                             questions,
                             choices,
                             choices.label.column.to.use=NULL){




  questionnaire<-koboquest::load_questionnaire(data = data,
                                               questions,
                                               choices,
                                               choices.label.column.to.use = choices.label.column.to.use)
}




#' Load an analysis plan from a csv file
#'
#' @param file path to a csv file with the analysis plan
#' @param df alternative to `file`, you can provide the analysis plan as a data frame
#' @details The analysis plan csv file must contain the following column headers:
#' "repeat.for.variable","research.question", "sub.research.question", "hypothesis", "independent.variable", "dependent.variable", "hypothesis.type", "independent.variable.type", "dependent.variable.type".
#' You can generate an empty template with
#' @export
load_analysisplan<-function(file=NULL,df=NULL){

if(!is.null(file) & !is.null(df)){stop("provide `file` or `df` parameters, not both")}
if(is.null(file) & is.null(df)){stop("provide one of `file` or `df` parameters")}

if(!is.null(file)){df <- read.csv.auto.sep(file)}
if(!is.null(df)){df <- df}

assert_valid_analysisplan(df)



# convert missing to NA, remove empty rows..:
ap_raw <- analysisplan_clean(df)

if(nrow(ap_raw)==0){stop("all rows in analysis plan are empty")}

missing_dependent_var<-ap_raw[["dependent.variable"]] %>% value_is_empty
if(any(missing_dependent_var)){
  stop(paste("all rows in the analysisplan must have an dependent variable. Missing rows:"))
}



return(ap_raw)
  }



assert_valid_analysisplan<-function(df){
  expected_column_names<-c("repeat.for.variable",
                           "research.question",
                           "sub.research.question",
                           "hypothesis",
                           "independent.variable",
                           "dependent.variable",
                           "hypothesis.type",
                           "independent.variable.type",
                           "dependent.variable.type")

  assertthat::assert_that(is.data.frame(df))
  expected_colnames_not_found <- expected_column_names[!(expected_column_names %in% colnames(df))]

  if(length(expected_colnames_not_found)){
    stop(paste("expected analysis plan columns not found:\n",
               paste(expected_colnames_not_found,collapse="\n"),"\n"

    ))
  }

}


value_is_empty<-function(x) {
  if(is.null(x)){return(TRUE)}
  return(x %in% c("",NA,"N/A","NA"))
}


remove_empty_rows<-function(df){
rows_empty<-apply(df,1,function(x){all(value_is_empty(x))})
  return(df[!rows_empty,,drop=F])
}


