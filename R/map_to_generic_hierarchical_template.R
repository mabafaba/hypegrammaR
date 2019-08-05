#' html from resultlist with results in specified hierarchical order based on analysisplan
#' @param resultlist structure like the output from from_analysisplan_map_to_output:
#' A list with two items "analysisplan" and "results": The "analysisplan" as a data frame, where each row must match a  result in a list of "results"
#' @param render_result_with a function that takes a single result as input and returns an rmarkdown formated string
#' @param by_analysisplan_columns vector of strings matching column names of the analysisplan. The first element becomes the main heading, the second element the sub-heading etc.
#' @param by_prefix a prefix added at the beginnig of the headline; same length as `by_analysisplan_columns`
#' @param level the markdown header level to start with; defaults to 2 which leads to "## heading", i.e. the second header level.
#' @param questionnaire optional; the questionnaire (koboquest::load_questionnaire())
#' @param label_varnames wether variables names should be labeled in headings
#' @param dir the directory in which to save the output file (absolute path or relative to current working directory)
#' @param type the type of report template to use. Currently one of "full", "visual" or "summary"
#' @param filename the name of the file. must end in '.html'
#' @export
map_to_generic_hierarchical_html <- function(resultlist,
                                          render_result_with,
                                          by_analysisplan_columns = c("dependent.var"),
                                          by_prefix = c("", "subset:", "variable:"),
                                          level = 2,
                                          questionnaire = NULL,
                                          label_varnames = TRUE,
                                          dir = "./",
                                          filename){

  assertthat::assert_that(assertthat::is.string(dir))
  assertthat::assert_that(assertthat::is.string(filename))
  assertthat::assert_that(assertthat::is.writeable(dir))
  assertthat::assert_that(is.function(render_result_with))
  assertthat::assert_that(length(by_analysisplan_columns) == length(by_prefix))
  if(!grepl(".html$",filename)){stop("file must end in .html")}

  template <-
        system.file("md_templates",
                    "generic_hierarchical_analysisplan_template.Rmd",
                    package = "hypegrammaR")

  render_environment <- new.env()

  render_environment$resultlist <- resultlist
  render_environment$by_analysisplan_columns = by_analysisplan_columns
  render_environment$by_prefix = by_prefix
  render_environment$level = level
  render_environment$render_result_with = render_result_with
  render_environment$questionnaire = questionnaire
  render_environment$label_varnames = label_varnames

  rmarkdown::render(
    template,
    output_file = filename,
    output_dir = dir,
    intermediates_dir = dir,
    envir = render_environment,
    knit_root_dir = getwd()
  )

  full_path<-dir %>% gsub("/$","",.) %>% gsub("^/","",.)
  full_path<-paste0(getwd(),full_path)
  message("document written to:")
  message(full_path)
  invisible(full_path)

}
