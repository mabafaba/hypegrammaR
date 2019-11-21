#' These are wrappers to run many analysis at once based on an analysis plan
#' In hypegrammaR, you identify a dependent variable, an independent variable, an analysis case and a hypothesis type.
#' The analyssiplan basically collects that information in a table that can then be applied in batch.
#'
#'
#'




#' expand an analysis plan with repeat variable
#'
#' @param analysisplan an analysis plan as a data frame
#' @param data a datasets matching the analysisplan
#'
#' @details an analysisplan can contain a "repeat.var" column.
#' This function expands each analysisplan row with a repeat variable into one row per unique value in the dataset for that variable;
#' for example if the repeat variable is "district", and in the data there are the districts  "d_1" and "d_2"
#' this will create a new analysisplan with two rows where "repeat.var.value" is "d_1" and "d_2" respectively
analysisplan_expand_repeat <- function(analysisplan, data) {
  if (!is.null(analysisplan[, "repeat.var"])) {
    # repeat.var.value <- unique(data[[repeat.var]])
    # repeat.var.value <- repeat.var.value[!is.na(repeat.var.value )]

    # store the ap rows that do not use repeat var
    analysisplan.no.repeat <-
      analysisplan[analysisplan$repeat.var %in% c(NA, "", " "), ]
    # set their repeat values to NA...
    if (!nrow(analysisplan.no.repeat) < 1) {
      analysisplan.no.repeat$repeat.var <- NA
      analysisplan.no.repeat$repeat.var.value <- NA
    } else{ # .. unless there are no rows that don't use repeat
      analysisplan.no.repeat$repeat.var <- character(0)
      analysisplan.no.repeat$repeat.var.value <- character(0)
    }
    # pick the row that do use repeat
    analysisplan.repeat <-
      analysisplan[!(analysisplan$repeat.var %in% c(NA, "", " ")), ]

    # for each row, expand to however many unique values exist in the data, create that many rows,
    # and fill the new "repeat.var.value" column with those values.
    analysisplan.repeat <-
      (1:nrow(analysisplan.repeat)) %>% lapply(function(ap_row_index) {
        ap_row <- analysisplan.repeat[ap_row_index, ] %>% unlist
        ap_row_expanded
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
#' @param verbose should progress be printed to the console? (default TRUE, slightly faster if FALSE)
#' @param confidence_level the confidence level to be used for confidence intervals (default: 0.95)
#' @return returns a list of hypegrammaR "result" objects (see map_to_result())
#' @export

from_analysisplan_map_to_output <- function(data,
           analysisplan,
           weighting = NULL,
           cluster_variable_name = NULL,
           questionnaire = NULL,
           labeled = FALSE,
           verbose = TRUE,
           confidence_level = 0.95) {


  #overwrite 'labeled' paramater if questionnaire is missing
  if(is.null(questionnaire)){labeled <- FALSE}

  # shorten analysisplan column names. This is because the analysis plan we used more human readable column names than in the code.
        analysisplan <-
          analysisplan %>% dplyr::rename(
            repeat.var = repeat.for.variable,
            dependent.var = dependent.variable,
            independent.var = independent.variable,
            dependent.var.type = dependent.variable.type,
            independent.var.type = independent.variable.type
          )
  # remove "junk" rows (if dependent.var is NA, it's definitely empty)
  # Since the analysisplan is often made in excel it can happen easily to have some rows that are empty or otherwise aren't meant to be used)
  analysisplan <- analysisplan[!is.na(analysisplan$dependent.var), ]
  # later we added more structured a generic function to clean up a raw analysisplan:
  analysisplan <- analysisplan_clean(analysisplan)

  # convert factors to characters because factors are the devil
  lapply(analysisplan,function(x){if(is.factor(x)){return(as.character(x))};x}) %>% as.data.frame(stringsAsFactors = FALSE)

  # each 'repeat.var' repetition gets their own row
  analysisplan <- analysisplan_expand_repeat(analysisplan, data)


  # add "percentcomplete" to print progress to console
    analysisplan$percentcomplete <-
      paste0(floor(1:nrow(analysisplan) / nrow(analysisplan) * 100), "%\n\n")

    # do analysis for each row in the analysis plan:
    results <- apply(analysisplan, 1, function(x) {

      # subset for current repition (if has a repeat var)
      if (!(x["repeat.var"]) %in% c(NULL, "", " ", NA) & !is.na(x["repeat.var"])) {
        this_valid_data <-
          data[(data[, as.character(x["repeat.var"])] == as.character(x["repeat.var.value"])), ]
      }else{
        this_valid_data <- data
      }

      # check data is still good:

      if(!(is_good_dataframe(this_valid_data))){
        result <- list()
        return(result)
      }


      # print progress:

      if(verbose){
      printparamlist(x, "calculating summary statistics and hypothesis tests")
      }

      if (is.na(x["independent.var"]) | is.null(x["independent.var"])) {
        indep.var <- NULL
      } else{
        indep.var <- as.character(x["independent.var"])
      }

      # identify analysis case:
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

      # calculate the results:


      result <- map_to_result(
        data = this_valid_data,
        dependent.var = x["dependent.var"],
        independent.var = indep.var, # own variable because was changed to null if NA
        case = case,
        cluster.variable.name = cluster_variable_name,
        weighting = weighting,
        questionnaire = questionnaire,
        confidence_level = confidence_level
      )

      # add into output the parameter information on the current repetetition / subset
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
      return(result)
    })

    # output not only the results but also the final analysisplan that was used,
    # where each analysisplan row matches one item in the result list
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
  cat(crayon::blue(paste("----  ", x["percentcomplete"])))
}

analysisplan_clean<-function(analysisplan){
  analysisplan<-lapply(analysisplan,function(x){
    x[value_is_empty(x)]<-NA
    x
  }) %>% as.data.frame(stringsAsFactors=F)
  analysisplan<-remove_empty_rows(analysisplan)
analysisplan
  }

