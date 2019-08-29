#
# #Summary statistics tests
# context("Hypothesis tests test")
#
# example<-load.example("example1",F)
# data <- example$data
# tf <- example$tf
# design <- svydesign(~0, data = data)
# questionnaire <- example$questionnaire
#
# ## hypothesis_test_chisquared select one
# test_that("hypothesis test chisquared sanitation works",{
#   expect_null(hypothesis_test_chisquared_select_one(tf$select_one_many_cat[2], tf$select_one[1],design = design)$results) ## too many categories for ChiSquared
#   expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one_many_cat[1],design = design)$name, "too many (>=30) unique values in independent variable")
#   expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one[1],design = design)$name, "Pearson's X^2: Rao & Scott adjustment")
#   })
#
#
# ##hypothesis_test_chisquared_select_multiple
#
# test_that("hypothesis test chisquared select multiple sanitation works",{
#   sm.columns <- questionnaire$choices_for_select_multiple(tf$select_multiple[1], data = data)
#   expect_is(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one[1], design = design), "list")
#   expect_equal(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one_many_cat[1], design = design)$name, "too many (>=20) unique values in independent variable")
#   expect_equal(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one_NA_heavy[1], design = design)$name %>% levels, "Pearson's X^2: Rao & Scott adjustment")})
#  # expect_error(hypothesis_test_chisquared_select_multiple(tf$select_multiple[2], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one[1], design = design)) ## doesnt throw an error because it essentially doesnt need the dependent var
#
#
#
# ## hypothesis test limit (t test one sample)
# test_that("Hypothesis test limit works",{
#   expect_gt(hypothesis_test_t_one_sample(tf$numeric[2], limit = 3,design = design)$result$t, -150) ## too many categories for ChiSquared
#   expect_equal(hypothesis_test_t_one_sample(tf$select_one_many_cat[1], limit = 3,design = design)$name, "dependent variable is not numeric")
#   expect_error(hypothesis_test_t_one_sample(tf$select_one[2], tf$select_one[1],design = design)$name, "argument \"limit\" is missing, with no default")
# })
#
# ## hypothesis test limit (t test one sample)
# test_that("Hypothesis test two sample works",{
#   expect_gt(hypothesis_test_t_two_sample(tf$numeric[2], tf$select_one[2],design = design)$result$t, 6.77) ## too many categories for ChiSquared
#   expect_equal(hypothesis_test_t_two_sample(tf$numeric[2], independent = tf$select_one_many_cat[1],design = design)$name, "too many (>=30) unique values in independent variable")
#   expect_equal(hypothesis_test_t_two_sample(tf$numeric[1], tf$select_one_NA_heavy[1],design = design)$name, "two sample ttest on difference in means (two sided)")
# })

# #Summary statistics tests
# context("Hypothesis tests test")
#
# example<-load.example("example1",F)
# data <- example$data
# tf <- example$tf
# design <- svydesign(~0, data = data)
# questionnaire <- example$questionnaire
#
# ## hypothesis_test_chisquared select one
# test_that("hypothesis test chisquared sanitation works",{
#   expect_null(hypothesis_test_chisquared_select_one(tf$select_one_many_cat[2], tf$select_one[1],design = design)$results) ## too many categories for ChiSquared
#   expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one_many_cat[1],design = design)$name, "too many (>=30) unique values in independent variable")
#   expect_equal(hypothesis_test_chisquared_select_one(tf$select_one[2], tf$select_one[1],design = design)$name, "Pearson's X^2: Rao & Scott adjustment")
#   })
#
#
# ##hypothesis_test_chisquared_select_multiple
#
# test_that("hypothesis test chisquared select multiple sanitation works",{
#   sm.columns <- questionnaire$choices_for_select_multiple(tf$select_multiple[1], data = data)
#   expect_is(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one[1], design = design), "data.frame")
#   expect_equal(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one_many_cat[1], design = design)$name, "too many (>=30) unique values in independent variable")
#   expect_is(hypothesis_test_chisquared_select_multiple(tf$select_multiple[1], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one_NA_heavy[1], design = design), "data.frame")
#   expect_warning(hypothesis_test_chisquared_select_multiple(tf$select_multiple[2], dependent.var.sm.cols = sm.columns, independent.var =  tf$select_one[1], design = design)) ## doesnt throw an error because it essentially doesnt need
# })
#
#
# ## hypothesis test limit (t test one sample)
# test_that("Hypothesis test limit works",{
#   expect_gt(hypothesis_test_t_one_sample(tf$numeric[2], limit = 3,design = design)$result$t, -150) ## too many categories for ChiSquared
#   expect_equal(hypothesis_test_t_one_sample(tf$select_one_many_cat[1], limit = 3,design = design)$name, "dependent variable is not numeric")
#   expect_error(hypothesis_test_t_one_sample(tf$select_one[2], tf$select_one[1],design = design)$name, "argument \"limit\" is missing, with no default")
# })
#
# ## hypothesis test limit (t test one sample)
# test_that("Hypothesis test two sample works",{
#   expect_gt(hypothesis_test_t_two_sample(tf$numeric[2], tf$select_one[2],design = design)$result$t, 6.77) ## too many categories for ChiSquared
#   expect_equal(hypothesis_test_t_two_sample(tf$numeric[2], independent = tf$select_one_many_cat[1],design = design)$name, "too many (>=30) unique values in independent variable")
#   expect_equal(hypothesis_test_t_two_sample(tf$numeric[1], tf$select_one_NA_heavy[1],design = design)$name, "two sample ttest on difference in means (two sided)")
# })
