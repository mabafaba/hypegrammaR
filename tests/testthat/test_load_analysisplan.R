#' context("load_analysis_plan()")
#'
#' expected_column_names<-c("repeat.var",
#'                          "research.question",
#'                          "sub.research.question",
#'                          "hypothesis",
#'                          "independent.var",
#'                          "dependent.var",
#'                          "hypothesis.type",
#'                          "independent.var.type",
#'                          "dependent.var.type")
#'
#'
#' test_ap<-lapply(expected_column_names,function(x){runif(10)}) %>% as.data.frame
#' colnames(test_ap)<-expected_column_names
#'
#'
#' testthat::test_that("load_analysis_plan throws errors when input corrupt",{
#'
#'   expect_error(load_analysisplan(NA))
#'   expect_error(load_analysisplan(NULL))
#'   expect_error(load_analysisplan(NA,NA))
#'   expect_error(load_analysisplan(1,1))
#'   expect_error(load_analysisplan(df=data.frame(a=runif(10),b=runif(10))))
#'   expect_error(load_analysisplan(df=data.frame("repeat.for"=NA)))
#'
#' })
#'
#' #' analysis plan template
#' #'
#' #' creates a data frame or csv file in the correct format for a hypegrammaR analysisplan
#' #'
#' #' @param file Optional: path and csv file name to write the empty analysis template to
#' create_empty_analysisplan<-function(file=NULL){
#'   expected_column_names<-c("repeat.for.variable",
#'                            "research.question",
#'                            "sub.research.question",
#'                            "hypothesis",
#'                            "independent.variable",
#'                            "dependent.variable",
#'                            "hypothesis.type",
#'                            "independent.variable.type",
#'                            "dependent.variable.type")
#'
#'   example_ap<-lapply(expected_column_names,function(x){NA}) %>% as.data.frame
#'   colnames(example_ap)<-expected_column_names
#'   example_ap<-example_ap[c(),,drop=F]
#'   if(!is.null(file)){
#'     example_ap %>% write.csv("empty_ap.csv")
#'   }
#'   (example_ap)
#' }
#'
