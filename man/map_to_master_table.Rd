% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mappings.R
\name{map_to_master_table}
\alias{map_to_master_table}
\title{Make the master table of summary stats and hypothesis tests}
\usage{
map_to_master_table(results_object, filename, questionnaire = NULL)
}
\arguments{
\item{results_object}{a list containing one or more hypegrammaR result objects: the output of map_to_result}

\item{filename}{The name of the file that is produced. The extension needs to be ".csv".}

\item{questionnaire}{optional: the questionnaire obtained by load_questionnaire. Necessary is you want labeled results}
}
\value{
a dataframe containing the summary statistics and p values for each element in results.
}
\description{
Make the master table of summary stats and hypothesis tests
}
