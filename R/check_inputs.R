#' #' Check if Assessment files are in order
#' #'
#' #' @param data the dataset ( from load_data() )
#' #' @param questionnaire the questionnaire ( from load_questionnaire() )
#' #' @param samplingframe the samplingframe ( from load_samplingframe() )
#' #' @param analysisplan
#' #' @export
#' check_input<-function(data,questionnaire,samplingframe,analysisplan){
#'
#' }
#'
#'
#' check_input_questionnaire<-function(questionnaire){
#'
#' }
#'
#' check_input_questionnaire_match_data<-function(questionnaire,data){
#'
#'   select_ones<-names(data)[sapply(names(data),questionnaire$question_is_select_one)]
#'   select_multiples<-names(data)[sapply(names(data),questionnaire$question_is_select_multiple)]
#'   numeric<-names(data)[sapply(names(data),questionnaire$question_is_numeric)]
#'
#'   d_not_found_in_q<-check_questionnaire_data_questions_found(questionnaire,data)
#'
#'
#'   select_one_questions<-questionnaire$question_is_select_one(names(data))
#'
#'   questionnaire$question_is_skipped(data,"asdf")
#'
#'   koboquest:::extract_all_varnames_from_condition()
#' }
#'
#'
#' check_questionnaire_data_questions_found<-function(questionnaire,data){
#'   names_found<-sapply(names(data),questionnaire$question_in_questionnaire)
#'   names_not_found<-names(names_found)[!names_found]
#'   not_found_but_is_sm_choice<-sapply(names_not_found,questionnaire$question_is_sm_choice)
#'   names_not_found<-names_not_found[!not_found_but_is_sm_choice]
#'   issues<-tibble(identifier=names_not_found,issue="data column not found in questionnaire")
#'   return(issues)
#' }
#'
#'
#'
#'
#'
#'
#' check_input_samplingframe_match_data<-function(samplingframe,data){
#' }
#'
#' check_input_samplingframe_match_questionnaire<-function(samplingframe,data){}
#'
#' check_input_questionnaire_match_analysisplan<-function(samplinframe,data){}
#'
#' check_input_analysisplan_match_data<-function(samplinframe,data){}
#'
#'
#'
#'
#'
