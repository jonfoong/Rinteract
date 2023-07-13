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

test_that("set single color for all cell values", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  plot <- int_graph(out, X1~X2+X3, col_values = "black")
  expect_true("ggplot" %in% class(plot))
})

test_that("manually setting label values work", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  plot <- int_graph(out, X1~X2+X3,
                    label_vals = list(
                      X1 = c(`0` = "None", `1` = "X1", "all" = "all", "effect" = "X1 effect"),
                      X2 = c(`0` = "None", `1` = "X2", "all" = "all", "effect" = "X2 effect"),
                      X3 = c(`0` = "None", `1` = "X3", "all" = "all", "effect" = "X3 effect")
                    ))
  expect_true("ggplot" %in% class(plot))
})

test_that("lab_both=FALSE works", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  plot <- int_graph(out, X1~X2+X3,
                    lab_both = FALSE)
  expect_true("ggplot" %in% class(plot))
})





