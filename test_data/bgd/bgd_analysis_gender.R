this_script_path<-(dirname(rstudioapi::getActiveDocumentContext()$path))
setwd(this_script_path)
setwd("./../..")
source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")

data<-load_assessment_bgd()

result_table<-map_list_of_results_to_dataframe(results)
