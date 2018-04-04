weight <- read.csv2("./weights.csv", header = T,stringsAsFactors = F )
weight


 rm(sampling.frame.stratum.column)
weight %>% lapply(typeof)
# weight <- weight[,c(1,2)]

strata <- "Camp_"
weightz<- na.omit(weight$Weight...National) 
strata1 <- na.omit(weight$Camp_)
# names(weight) <- c("sampling.frame.stratum.column", "weights")

weight[[weightz]] %>% as.numeric -> weights
weight[[sampling.frame.stratum.column]] %>% typeof



new_data <- read.csv2("./IRQCCCM.csv", header = T)

W <- as.vector(weight$weights)
rm(weights)

design <- svydesign(ids =~0, probs=NULL, strata = strata1, data = new_data, weights = weights)
survey:::svydesign

sf_raw<-read.csv2("./weights.csv",stringsAsFactors = F, header = T)
# create unique strata names from sampling frame
unique_strata <- sf_raw[, sampling.frame.stratum.column]

(unique_strata %>% hasdata %>% table)
