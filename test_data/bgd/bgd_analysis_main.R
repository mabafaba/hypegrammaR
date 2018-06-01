source("./test_data/bgd/bgd_analysis_data_load_and_prepare.R")
#####







result_table$indicator <- gsub("VAR\\.[0-9]*\\.\\.\\.","",result_table$indicator)
result_table$indicator <- gsub("\\."," ",result_table$indicator)


split_by_gender<-result_table %>% split.data.frame(result_table$independent.var.value)


gender_as_cols<-data.frame("Indicator"=split_by_gender$Female$indicator,
      "Answer choice"=split_by_gender$Female$dependent.var.value,
      "p-Value" = split_by_gender$Female$`p value`,
      "test name" = split_by_gender$Female$`test type`,
      "Number Female HHH" = split_by_gender$Female$numbers,
      "Numbers Male HHH"=split_by_gender$Male$numbers) %>% head


gender_as_cols$absolut.difference<-abs(gender_as_cols$Numbers.Male.HHH-gender_as_cols$Number.Female.HHH)

gender_as_cols %>% write.csv("male_vs_female.csv")


gender_as_cols %>% glimse
gender_as_cols %>% aggregate.data.frame(list(gender_as_cols))


