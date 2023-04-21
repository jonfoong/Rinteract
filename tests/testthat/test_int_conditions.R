library(testthat)
library(Rinteract)
library(estimatr)


# test different models

test_that("lm works", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata)
  expect_s3_class(out, "data.frame")
})

test_that("glm works", {
  mod <- glm(Y~X1*X2*X3*X4,
             toydata |>
               transform(Y = ifelse(Y>0, 1, 0)), family = "binomial")
  out <- int_conditions(mod, toydata)
  expect_s3_class(out, "data.frame")
})


test_that("lm_robust works", {
  mod <- lm_robust(Y~X1*X2*X3*X4, toydata)
  out <- int_conditions(mod, toydata)
  expect_s3_class(out, "data.frame")
})

# test conmeans FALSE

test_that("argument conmeans = FALSE", {
  mod <- lm(Y~X1*X2*X3, toydata)
  out <- int_conditions(mod, toydata, conmeans = FALSE)
  expect_s3_class(out, "data.frame")
})

# test specifying main vars
test_that("specify main vars", {
  mod <- lm(Y~X1*X2 + X3*X4, toydata)
  out <- int_conditions(mod, toydata, main_vars = c("X3", "X4"), .names = c(A1="X3", A2 = "X4"))
  expect_true(all(c("A1", "A2") %in% colnames(out)))
})

# test specifying prediction vars

test_that("specify prediction vars", {
  mod <- lm(Y~X1*X2 + X3*X4, toydata)
  out <- int_conditions(mod, toydata,
                                 pred_vars = data.frame(X1 = 500, X2 = 6),
                                 main_vars = c("X3", "X4"))
  expect_s3_class(out, "data.frame")
})


# test fixed effects for lm_robust
test_that("fixef works", {
  mod <- lm_robust(Y~X1*X2*X3*X4, toydata, fixed_effects = ~FE)
  out <- int_conditions(mod, toydata, fixef = list(FE = factor(1:5)))
  expect_s3_class(out, "data.frame")
})







