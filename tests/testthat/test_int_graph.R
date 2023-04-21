library(testthat)
library(Rinteract)

test_that("no facet triggers error", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  expect_error(int_graph(out), "facet argument cannot be empty!")
})


test_that("int_graph returns ggplot object", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  plot <- int_graph(out, X1+X2~X3)
  expect_true("ggplot" %in% class(plot))
})






