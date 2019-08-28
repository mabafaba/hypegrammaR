#' Map results to an output template
#'
#' @param x hypegrammaR result or list of results (created with map_to_result() or from_analysisplan_map_to_output())
#' @param questionnaire optional: the questionnaire (load_questionnaire())
#' @param dir the directory in which to save the output file (absolute path or relative to current working directory)
#' @param type the type of report template to use. Currently one of "full", "visual" or "summary"
#' @param filename the name of the file. must end in '.html'
#' @export
map_to_template <- function(x, questionnaire = NULL, dir, type = NULL, filename,custom_template = NULL) {

  if(is.null(type)&is.null(custom_template)){stop("must provide either type or custom_template arguments")}
  if(!is.null(type)&!isnull(custom_template)){stop("most provide only one of type or custom_template arguments")}

  if(!is.null(custom_template)){
    template<-custom_template
  }

  if(!is.null(type)){

  type <-recode(type,
                visual= "templates_analysisplan_report_visuals.rmd",
                summary ="templates_analysisplan_summary.rmd",
                full = "templates_analysisplan_report_full.rmd")

  if (class(x) == "hypegrammar_resultlist") {
    template <-
      system.file("md_templates",
                  type,
                  package = "hypegrammaR")
  }

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

  full_path<-dir %>% gsub("/$","",.) %>% gsub("^/","",.)
  full_path<-paste0(getwd(),full_path)
  message("document written to:")
  message(full_path)
  invisible(full_path)

  }
