#' Map results to an output template
#'
#' @param x hypegrammaR result or list of results (created with map_to_result() or from_analysisplan_map_to_output())
#' @param questionnaire optional: the questionnaire (load_questionnaire())
#' @parm dir the directory in which to save the output file (absolute path or relative to current working directory)
#' @param filename the name of the file. must end in '.html'
#' @export
map_to_template <- function(x, questionnaire = NULL, dir, filename) {
  if (class(x) == "hypegrammar_resultlist") {
    template <-
      system.file("md_templates",
                  "templates_analysisplan_report.rmd",
                  package = "hypegrammaR")
  }
  render_environment <- new.env()

  render_environment$x <- x
  render_environment$questionnaire <- questionnaire
  rmarkdown::render(
    template,
    output_file = filename,
    output_dir = dir,
    intermediates_dir = dir,
    envir = render_environment,
    knit_root_dir = getwd()
  )
  path<-"/asdasd/sd/"
  full_path<-path %>% gsub("/$","",.) %>% gsub("^/","",.)
  full_path<-paste0(getwd(),full_path)
  message("document written to:")
  message(full_path)
  invisible(full_path)

  }
