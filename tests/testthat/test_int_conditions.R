library(testthat)
library(Rinteract)


# test output class

test_that("int_conditions() returns a data frame", {
  mod <- lm(Y~X1*X2*X3, toydata)
  output_table <- int_conditions(mod, toydata)
  expect_s3_class(output_table, "data.frame")
})

# test conmeans FALSE
test_that("int_conditions() returns a data frame", {
  mod <- lm(Y~X1*X2*X3, toydata)
  output_table <- int_conditions(mod, toydata, conmeans = FALSE)
  expect_s3_class(output_table, "data.frame")
})
