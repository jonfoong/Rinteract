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

# test that all estimates are correct

test_that("all estimates from output correct", {
  data <- toydata
  mod <- lm(Y~X1*X2*X3*X4, data)
  out <- int_conditions(mod, data, conmeans = FALSE)
  terms <- names(coef(mod))[-1]
  coef_df <- as.data.frame(t(mod$coefficients))

  checks <- sapply(1:nrow(out), function(x){

    df <- out[x,]
    estimate <- df$estimate
    vars <- setdiff(colnames(df),
                    c("estimate", "std.error", "p.value", "value"))

    eff_vars <- colnames(df)[df=="effect"]
    con_vars <- setdiff(vars, eff_vars)

    effect <- paste0("`", terms[Rinteract:::ind_fn(terms, eff_vars)], "`")

    con_effect <- unlist(sapply(con_vars, function(y){

      eff <- df[[y]]

      full_term <- c(y, eff_vars)

      full_term <- terms[Rinteract:::ind_fn(terms, full_term)]

      if (eff!=0){

        value <- as.character(ifelse(eff=="all",
                        mean(data[[y]], na.rm=TRUE),
                        eff))
        sprintf("%s*`%s`", value, full_term)

      }

    }))

    if (is.null(con_effect)){
      form <- effect
    } else {
      form <- paste(c(effect, paste(con_effect, collapse = "+")), collapse = "+")
    }

    value <- transform(coef_df, value = eval(parse(text = form)))[["value"]]
    value==estimate
  })

  #expect_null(out[!checks,])

  #expect_gt(sum(checks), 174)

  #expect_true(all(checks))

  expect_no_match(paste(which(!checks), collapse = ", "), ".+")

})







