#Summary statistics tests
context("Hypothesis tests test")

example<-load.example("example1",F)
data <- example$data
tf <- example$tf
design <- svydesign(~0, data = data)

## hypothesis_test_chisquared select one
test_that{
  expect_null(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one[1],design = design)$results) ## too many categories for ChiSquared
  expect_message(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one[1],design = design), "")
}


##hypothesis_test_chisquared_select_multiple

## hypothesis test limit (t test one sample)

hypothesis_test_t_one_sample
