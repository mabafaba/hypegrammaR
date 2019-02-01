setwd("./hypegrammaR/")
require("devtools")
usethis::use_testthat()
usethis::use_build_ignore("build_package.R")
usethis::use_build_ignore(".*\\.R")




# data<-read.csv("../msna18/internal/input_files/data.csv")
#
# q<-koboquest::load_questionnaire(data,
#                                     questions.file = "../msna18/internal/input_files/kobo questions.csv",
#                                     choices.file = "../msna18/internal/input_files/kobo choices.csv"
#                                     )
# data
# question_is_categorical("village")
# require("koboquest")
#
# analyse_indicator(data = data,
#                   dependent.var = "dependencyratio",
#                   independent.var = "sum_nutrition_need",
#                   hypothesis.type = "group_difference",sampling.strategy.cluster = F,case = "CASE_group_difference_categorical_categorical",sampling.strategy.stratified = F)
#
#
#
#


