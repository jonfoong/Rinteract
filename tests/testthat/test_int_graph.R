library(testthat)
library(Rinteract)
library(ggplot2)
library(ggh4x)

test_that("no facet triggers error", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  expect_error(int_graph(out), "facet argument cannot be empty!")
})






