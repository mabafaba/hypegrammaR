#
#     # shorten column headers

#
#     cases <- apply(analysisplan, 1, function(x) {
#       # print("\n\n\n")
#       # print(x %>% kable)
#
#       list(case)
#     }) %>% lapply(unlist)
#
#   }
#





#' expand an analysis plan with repeat var
#'
#' each repetion gets its own analysisplan row
analysisplan_expand_repeat <- function(analysisplan, data) {
  if (!is.null(analysisplan[, "repeat.var"])) {
    # repeat.var.value <- unique(data[[repeat.var]])
    # repeat.var.value <- repeat.var.value[!is.na(repeat.var.value )]
    analysisplan.no.repeat <-
      analysisplan[analysisplan$repeat.var %in% c(NA, "", " "), ]
    if (!nrow(analysisplan.no.repeat) < 1) {
      analysisplan.no.repeat$repeat.var <- NA
      analysisplan.no.repeat$repeat.var.value <- NA
    } else{
      analysisplan.no.repeat$repeat.var <- character(0)
      analysisplan.no.repeat$repeat.var.value <- character(0)
    }
    analysisplan.repeat <-
      analysisplan[!(analysisplan$repeat.var %in% c(NA, "", " ")), ]
    analysisplan.repeat <-
      (1:nrow(analysisplan.repeat)) %>% lapply(function(ap_row_index) {
        ap_row <- analysisplan.repeat[ap_row_index, ] %>% unlist
        ap_row_expanded <-
          matrix(
            ap_row,
            nrow = length(unique(data[[ap_row["repeat.var"]]])),
            ncol = length(ap_row),
            byrow = TRUE
          ) %>% as.data.frame(stringsAsFactors = F)
        colnames(ap_row_expanded) <- names(ap_row)
        ap_row_expanded$repeat.var.value <-
          unique(as.character(data[[ap_row["repeat.var"]]]))
        ap_row_expanded
      }) %>% do.call(rbind, .)

    # analysisplan.repeat <- lapply(repeat.var, function(x){
    #   if(!x %in% c(NA, "", " ")){
    #   repeat.var.value <- unique(data[[x]])
    #   repeat.var.value <- repeat.var.value[!is.na(repeat.var.value )]
    #   analysisplan %>% filter(repeat.var %in% x) %>% slice(rep(1:n(), each = length(repeat.var.value))) %>% cbind(.,repeat.var.value, stringsAsFactors = F)}}) %>% do.call(rbind,.)

    analysisplan <-
      rbind(analysisplan.no.repeat,
            analysisplan.repeat,

            stringsAsFactors = F)
  }
}

#' apply an analysis plan
#'
#' Takes all usual hypegrammaR input files plus an analysis plan and maps directly to an output document
#'
#' @param data the data set as a data frame (load_data())
#' @param analysisplan the analysisplan (load_analysisplan())
#' @param weighting optional: the weighting function (use load_samplingframe() and then map_to_weighting())
#' @param cluster_variable_name optional: the name of the variable with the cluster IDs
#' @param questionnaire optional: the questionnaire (load_questionnaire())
#' @param labeled do you want the resuts to display labels rather than xml names ? defaults to false, requires the questionnaire
#' @param should progress be printed to the console? (default TRUE, slightly faster if FALSE)
#' @return returns a list of hypegrammaR "result" objects (see map_to_result())
#' @export

from_analysisplan_map_to_output <- function(data,
           analysisplan,
           weighting = NULL,
           cluster_variable_name = NULL,
           questionnaire = NULL,
           labeled = FALSE,
           verbose = TRUE) {


  #overwrite 'labeled' paramater if questionnaire is missing
  if(is.null(questionnaire)){labeled <- FALSE}

  # shorten analysisplan column names
        analysisplan <-
          analysisplan %>% dplyr::rename(
            repeat.var = repeat.for.variable,
            dependent.var = dependent.variable,
            independent.var = independent.variable,
            dependent.var.type = dependent.variable.type,
            independent.var.type = independent.variable.type
          )
  # remove junk rows:
  analysisplan <- analysisplan[!is.na(analysisplan$dependent.var), ]
  analysisplan <- analysisplan_clean(analysisplan)

  # each 'repeat.var' repetition gets their own row
  analysisplan <- analysisplan_expand_repeat(analysisplan, data)


  # add "percentcomplete" to print progress to console
    analysisplan$percentcomplete <-
      paste0(floor(1:nrow(analysisplan) / nrow(analysisplan) * 100), "%\n\n")

    # Apply rows:
    results <- apply(analysisplan, 1, function(x) {

      # subset for current repition (if has a repeat var)
      if (!(x["repeat.var"]) %in% c(NULL, "", " ", NA)) {
        this_valid_data <-
          data[(data[, as.character(x["repeat.var"])] == as.character(x["repeat.var.value"])), ]
      }else{
        this_valid_data <- data
      }

      if(!(is_good_dataframe(this_valid_data))){
        result <- list()
        return(result)
      }


      # subset where dependent and independent has data
#
#       this_valid_data <- this_valid_data[which(!(is.na(this_valid_data[, as.character(x["dependent.var"])]))), ,drop=FALSE]
#       if (!is.na(x["independent.var"])) {
#         this_valid_data <- this_valid_data[which(!(is.na(this_valid_data[, as.character(x["independent.var"])]))), ]
#       }

      # print what we're doing to console


      if(verbose){
      printparamlist(x, "calculating summary statistics and hypothesis tests")
      }

      if (is.na(x["independent.var"]) | is.null(x["independent.var"])) {
        indep.var <- NULL
      } else{
        indep.var <- as.character(x["independent.var"])
      }
      case <- map_to_case(
        hypothesis.type = x["hypothesis.type"],
        dependent.var.type =
          if (value_is_empty(x["dependent.var.type"]))
            NULL
        else {
          x["dependent.var.type"]
        },
        independent.var.type =
          if (value_is_empty(x["independent.var.type"]))
            NULL
        else {
          x["independent.var.type"]
        }

      )

      result <- map_to_result(
        data = this_valid_data,
        dependent.var = x["dependent.var"],
        independent.var = indep.var, # own variable because was changed to null if NA
        case = case,
        cluster.variable.name = cluster_variable_name,
        weighting = weighting,
        questionnaire = questionnaire
      )

      if (!is.null(x["repeat.var"]) & (!is.na(x["repeat.var"]))) {
        result$parameters$repeat.var <- x["repeat.var"]
        result$parameters$repeat.var.value <-
          x["repeat.var"]
      } else{
        result$parameters$repeat.var <- NA
        result$parameters$repeat.var.value <- NA

      }

      if(labeled){
        result <- map_to_labeled(result, questionnaire)
      }



      if (!is.null(result$summary.statistic)) {
        if (nrow(result$summary.statistic) > 0) {
          result$summary.statistic$repeat.var <- x["repeat.var"]
          result$summary.statistic$repeat.var.value <-
            x["repeat.var.value"]
        } else{
          result$summary.statistic$repeat.var <- character(0)
          result$summary.statistic$repeat.var.value <- character(0)
        }
      }
      # names(result) <- paste(x["dependent.var"], result$parameters$repeat.var.value)
      return(result)
    })
    all_results<-list(results = results, analysisplan = analysisplan)
    class(all_results)<-"hypegrammar_resultlist"
    return(all_results)

  }




printparamlist <- function(x, title = "") {
  cat("\014")
  cat(title)
  cat("\n")
  cbind(names(x[-length(x)]), as.matrix(x)[-length(x)]) %>% apply(1, paste, collapse =
                                                                    " = '") %>% paste(collapse = "'\n") %>% cat
  cat("\n\n")
  cat(blue(paste("----  ", x["percentcomplete"])))
}

analysisplan_clean<-function(analysisplan){
  analysisplan<-lapply(analysisplan,function(x){
    x[value_is_empty(x)]<-NA
    x
  }) %>% as.data.frame(stringsAsFactors=F)
  analysisplan<-remove_empty_rows(analysisplan)
analysisplan
  }

