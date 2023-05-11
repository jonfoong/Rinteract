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

# test that all estimates are correct - testing against manual calculation
# make sure manual calculation is correct!!

test_that("all estimates from output correct", {

  data <- toydata
  mod <- lm(Y~X1*X2*X3*X4, data)
  out <- int_conditions(mod, data, conmeans = FALSE)
  terms <- names(coef(mod))[-1]
  coef_df <- as.data.frame(t(mod$coefficients))

  vars <- setdiff(colnames(out),
                  c("estimate", "std.error", "p.value", "value"))

  checks <- sapply(1:nrow(out), function(x){

    df <- out[x,]

    for (i in vars){

      value <- df[[i]]

      value <- ifelse(value=="effect", 1,
                      ifelse(value=="all", mean(data[[i]], na.rm = TRUE), value))

      assign(i, as.numeric(value))

    }

    eff_vars <- colnames(df)[df=="effect"]
    zero_vars <- colnames(df)[df==0]
    con_vars <- setdiff(vars, c(eff_vars, zero_vars))

    effect <- paste0("`", terms[Rinteract:::ind_fn(terms, eff_vars)], "`")

    if (length(con_vars)==0){

      form <- effect

    } else {
      con_vars <-
        do.call(c, lapply(1:length(con_vars),
                          function(x) combn(con_vars, x, FUN = function(x) list(c(eff_vars, x)))))

      con_effect <- unlist(sapply(con_vars, function(y){

        vars <- unlist(y)

        full_term <- terms[Rinteract:::ind_fn(terms, vars)]

        value <- eval(parse(text = paste(vars, collapse = "*")))

        sprintf("%f*`%s`", value, full_term)

      }))

      form <- paste(c(effect, paste(con_effect, collapse = "+")), collapse = "+")

    }

    value <- transform(coef_df, value = eval(parse(text = form)))[["value"]]

    return(round(value, 5))

  })

  estimates <- round(out$estimate, 5)

  expect_no_match(paste(which(checks!=estimates), collapse = ", "), ".+")
  #expect_no_match(paste(checks[checks!=out$estimate], collapse = ", "), ".+")

})

test_that("all output estimates match demeaned regression", {


  data <- toydata
  mod <- lm(Y~X1*X2*X3*X4, data)
  out <- int_conditions(mod, data, conmeans = FALSE)
  vars <- setdiff(colnames(out),
                  c("estimate", "std.error", "p.value", "value"))

  out <- out[matrix(!as.matrix(out[vars]) %in% c(0,1),
                    ncol=length(vars)) |>
               rowSums() == length(vars),]

  # now demean and rerun
  data_dm <- toydata |>
    transform(X1 = X1 - mean(X1),
              X2 = X2 - mean(X2),
              X3 = X3 - mean(X3),
              X4 = X4 - mean(X4))

  mod_dm <- lm(Y~X1*X2*X3*X4, data_dm)
  terms <- names(coef(mod_dm))[-1]

  dm_df <- data.frame(vars = names(mod_dm$coefficients),
                      mod_est = as.vector(round(mod_dm$coefficients, 5)))[-1,]

  out_df <- Rinteract:::callapply("rbind", 1:nrow(out), function(x){

    df <- out[x,]

    term_vars <- vars[df[vars]=="effect"]

    full_term <- terms[Rinteract:::ind_fn(terms, term_vars)]

    data.frame(vars = full_term, out_est = round(df$estimate, 5))

  })

  merge_df <- merge(out_df, dm_df) |>
    transform(check = out_est==mod_est)

  expect_true(all(merge_df$check))

})
