#bias estimation
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
source("./R/hypothesis_tests.R")
source("./R/mappings.R")
source("./R/summary_statistics.R")

library(dplyr)
library(data.table)
library(reachR)
data_ref <- reachR:::read.csv.auto.sep("./test_data/Kampala_AGORA_HH_Refugees_10052018.csv")
data_ref <- data_ref[c(1:705),] 
data_ref %>% tail
r <- c()

data_ref$feel.safe

split.value <- "sample"
weight.column <- "weight"
comparison <- aggregate_percent_weighted(df = data_ref, weight.by = weight.column, split.by = split.value, write.to.file = "./comparison_bias_percent.csv")

### Weighted comparisons
random_ref <- data_ref %>% subset(data_ref$sample == "random GPS")
non_random_ref <- data_ref %>% subset(data_ref$sample == "refugees")
percent_weighted_random <- aggregate_percent_weighted(df = random_ref, weight.by = weight.column)
R <- as.data.frame(unlist(percent_weighted_random))
percent_weighted_non_random <- aggregate_percent_weighted(df = non_random_ref, weight.by = weight.column)
NR <- as.data.frame(unlist(percent_weighted_non_random))
##### END

data_ref$toilet.safe

independent.var <- "sample"
dependent.var <- "toilet.safe"

#correction for numeric 
data_ref[[dependent.var]] <- as.numeric(gsub(",", ".",data_ref[[dependent.var]]))
which(is.na(data_ref[[dependent.var]]))
data_ref[[dependent.var]] %>% table


#weighting
weights_ref <- as.numeric(gsub(",", ".", data_ref$weight))
strata_ref <- data_ref$name.slum
library(survey)
design <- svydesign(ids=~0, weights = weights_ref, strata = strata_ref, data = data_ref, na.rm = T)

#num
r.hyp <- hypothesis_test_t_two_sample(independent.var = independent.var, dependent.var = dependent.var, design)
r.sum <- confidence_intervals_num_groups(independent.var = independent.var, dependent.var = dependent.var, design)


#cat
r.hyp <- hypothesis_test_chisquared(independent.var = independent.var, dependent.var = dependent.var, design)
r.sum <- percent_with_confints(independent.var = independent.var, dependent.var = dependent.var, design)


results.df.master <- data.frame(rep(dependent.var, times = length(r.sum$numbers)), r.sum$names, r.sum$numbers, 
                                rep(r.hyp$test.results[2], times = length(r.sum$numbers)), 
                                rep(r.hyp$hypothesis_test, times = length(r.sum$numbers)))
names(results.df.master) <- c("variable", "group.answer", "value", "p value", "test")

data_ref[[dependent.var]][data_ref[[dependent.var]] == "not_applicable"] <- NA
data_ref$hh.head.sex %>% table

results.df.master.n <- rbind(results.df.master.n, results.df.master)

results.df.master.n

write.csv(results.df.master.n, file = "random_purposive.new.csv")

unlist(r.hyp)
r.sum

#Categorical_results
r.n <- c(dependent.var, unlist(r.hyp), sum.r)
r <- rbind(r, r.n)

case <- 'CASE_group_difference_numerical_categorical'
summary.statistics <- confidence_intervals_num_groups(dependent.var = dependent.var, independent.var, design = design, data = data_ref)
hypothesis.test.results <- hypothesis_test_t_two_sample(independent.var = independent.var, dependent.var = dependent.var, design)


#####################


make.data.frames <- function(comparicion){
      x <- lapply(comparicion, function(t){
      as.data.frame(t)})
      return(x)}

df.results <- make.data.frames(comparison)

calculate.differences <- function(some){
  diff <- lapply(some, function(h){
    if(ncol(h) == 2){
    abs(as.vector(h[,1]) - as.vector(h[,2]))}else{print("hi")}})
    return(diff)}

differences <- calculate.differences(df.results)

summary(differences)

d <- sapply(ya, function(x){d <- which(ncol(x) == 2)})
summary(d)

differnces <- xo(ya)
#figure out the bias function better 
