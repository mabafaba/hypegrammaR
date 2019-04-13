#Summary statistics tests
context("Hypothesis tests test")

example<-load.example("example1",F)
data <- example$data
tf <- example$tf
design <- svydesign(~0, data = data)

## hypothesis_test_chisquared select one
test_that{
  expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one[1],design = design)$results, "data.frame")
}


##hypothesis_test_chisquared_select_multiple
