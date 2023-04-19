#' @title Generate conditional means and effects from a model with interactions
#' @description Conducts hypothesis testing across all conditions of interaction effects of a fitted model
#'
#' @param mod A model object
#' @param data Dataset used when fitting model
#' @param main_vars A vector of variable names in the interaction of interest. If unspecified, takes by default variables from the highest order interaction
#' @param .names A named vector for renaming variables
#' @param pred_vars int_conditions cannot return model predictions if there exists other variables beyond the interaction terms that are not supplied. To generate predictions from these models, supply a named dataframe of dimension 1*n, where n is the number of missing variables. Column names must correspond to terms used in model supplied and column values must be a singular numerical value. Defaults to 0 for all non-interaction variables.
#' @param fixef Are there fixed effects within the model? If so these must be supplied in order for predictions to be generated. The argument takes a list of named factors and generates predictions across the mean of all combinations of fixed effects.
#'
#' @return A data frame object that contains all effects, respective conditions, and estimated hypotheses
#' @examples
#' library(estimatr)
#' set.seed(1)
#' dat <- data.frame(X1 = sample(0:1, 100, replace=TRUE), X2 = sample(0:1, 100, replace=TRUE), X3 = sample(0:1, 100, replace=TRUE), FE = rep(1:5, 20))
#' dat <- dat |> transform(Y = X1 + 2*X2 + 3*X1*X2 + X3 + rnorm(1))
#' mod <- lm_robust(Y~X1*X2, dat, fixed_effects = ~FE)
#' cond_tab <- int_conditions(mod, data = dat, main_vars = c("X1", "X2"), .names = c(A1 = "X1", A2 = "X2"), fixef = list(FE = factor(1:5)))
#'
#' @export
#' @importFrom multcomp glht
#' @importFrom broom tidy

int_conditions <- function(mod,
                           data = NULL,
                           conmeans = TRUE, # if TRUE, returns all conditional means
                           main_vars = NULL,
                           .names = NULL, # takes a named vector; if specified, renames variables of model
                           pred_vars = NULL,
                           fixef = NULL # takes a list of named factors of fixed effects and their levels
){

  # stop if model input not accepted

  if (!any(class(mod) %in% c("lm", "glm", "lm_robust"))) stop("Only lm, lm_robust, and glm models accepted")

  # extract all vars from model

  all_vars <-
    attributes(mod$terms)$term.labels

  # if main_vars not specified, extracts highest order interaction

  if(is.null(main_vars)){

    ind <- which.max(sapply(all_vars,
                            function(x) length(strsplit_vec(x, ":"))))

    full_term <- all_vars[ind]

    main_vars <-
      strsplit_vec(full_term, ":")

  } else full_term <- paste(main_vars, collapse = ":")


  # how many terms?

  interacts <- length(main_vars)

  # exclude those we are not conditioning on

  vars <-
    all_vars[sapply(all_vars,
                    function(x) all(strsplit_vec(x, ":") %in% main_vars))]


  df_effects <-
    callapply("rbind", vars, function(x){

      term <- strsplit_vec(x, ":")

      marginal <- vars[sapply(strsplit(vars, ":"),
                              function(x) all(term %in% x))]

      data.frame(marginal = marginal, term = x)

    })

  # create a matrix with all values as variable names

  df_mat <-
    callapply("rbind", 1:nrow(df_effects), function(x) t(vars))

  colnames(df_mat) <- vars

  # create a sparse matrix of 0s and 1s to feed into our hypothesis testing

  df_effects <-
    callapply("rbind", 1:nrow(df_effects), function(x){

      df <- df_effects[x,]
      mat <- df_mat[x,]

      new_mat <-
        sapply(vars,
               function(y){

                 vec <- strsplit_vec(mat[[y]], ":")

                 ifelse(all(vec %in% strsplit_vec(df$marginal, ":")) &
                          all(strsplit_vec(df$term, ":") %in% vec),
                        1, 0)
               })

      new_df <- cbind(df, rbind(new_mat))
      return(new_df)

    })

  # now extract coeffs and perform hypothesis testing

  cond_effs <-
    callapply("rbind", 1:nrow(df_effects), function(x){

      dat <- df_effects[x,]

      # extract all marginal terms

      form_terms <-
        colnames(dat)[apply(dat==1, 2, all)]

      #extract all marginal terms in the formula

      main_terms <-
        strsplit_vec(dat$term, ":")

      marg_terms <-
        setdiff(unique(strsplit_vec(form_terms, ":")), main_terms)

      #create a grid of main terms first

      grid <-
        eval(parse(text = paste0("data.frame(",
                                 paste(paste0(main_terms, "=1"),
                                       collapse = ","), ")")))

      # if there are marginal conditions, create matrix of permutations and add back into grid

      if(length(marg_terms)!=0){

        marg_grid <-
          eval(parse(text = paste0("expand.grid(",
                                   paste(paste0(marg_terms, "=c(0.5,1)"),
                                         collapse = ","), ")")))
        grid <- cbind(marg_grid, grid)

      }

      # demean, replace 0.5 with means

      hyp_grid <- callapply("cbind", colnames(grid),
                            function(x) ifelse(grid[x]==0.5, mean(data[[x]]), 1))

      # now create df of all hypotheses we have to test incl formula

      hyp_tests <-
        callapply("rbind", 1:nrow(hyp_grid), function(x){

          if (nrow(hyp_grid)==1) grid_row <- hyp_grid else grid_row <- hyp_grid[x,]

          for (i in colnames(grid_row)) assign(i, grid_row[[i]])

          form <- paste0(paste(sapply(form_terms, function(x){

            val <- eval(parse(text = gsub("\\:", "\\*", x)))

            sprintf("%s*%s", val, x)
          }),
          collapse = " + "), " = 0")

          transform(grid_row, form = form)

        })

      # now we run hypotheses one by one

      all_hyps <-
        callapply("rbind", 1:nrow(hyp_tests), function(x){

          dat_hyp <- hyp_tests[x,]


          #run linear hypothesis using formula

          test <- tidy(glht(mod, dat_hyp$form))[c("estimate", "std.error", "adj.p.value")]

          colnames(test) <- c("estimate", "std.error", "p.value")

          # extract all conditions

          # get conditions for when x=1 or mean

          remn <- dat_hyp[!colnames(dat_hyp) %in% c("form", strsplit_vec(dat$term, ":"))]

          if(length(remn)!=0)
            remn <- sapply(colnames(remn),
                           function(x) paste(x, "=", ifelse(remn[x] %in% 0:1, remn[x] , "'all'")))

          # write test here to check for length of con_mean!

          con_mean <-
            unique(c(remn, paste(strsplit_vec(dat$term, ":"), "=1")))

          con_mean <-
            eval(parse(text = paste0("data.frame(",
                                     paste(con_mean, collapse=","), ")")))
          # get all zero conditions

          zero_names <-
            setdiff(main_vars, names(con_mean))

          con_zero <-
            matrix(0, 1, length(zero_names))

          colnames(con_zero) <- zero_names

          conditions <-
            cbind(con_mean, con_zero)

          # build dataframe

          conditions$effect <- dat$term

          conditions <- cbind(conditions, test)

          return(conditions)

        })
    })

  # rename var columns

  cond_effs <- callapply("rbind", 1:nrow(cond_effs), function(x){

    eff_df <- cond_effs[x,]
    effect <- eff_df$effect
    eff_df[eff_df==1 & colnames(eff_df) %in% strsplit_vec(effect, ":")] <- "effect"

    return(eff_df)
  })

  cond_effs <- cond_effs[setdiff(names(cond_effs), "effect")]
  cond_effs <- transform(cond_effs, value = "Causal effect")

  # generate conditional means from predictions

  if(conmeans){

    # first get all permutations

    all_pred <-
      callapply("expand.grid", main_vars, function(x) c(0, 1, "all"))

    all_pred <- as.data.frame(apply(all_pred, 2, as.character))

    colnames(all_pred) <- main_vars

    # get means

    # add test here for predictions

    df_pred <- all_pred

    for(i in colnames(df_pred)) df_pred[df_pred[i]=="all", i] <- mean(data[[i]])

    df_pred <- as.data.frame(apply(df_pred, 2, as.numeric))

    # attach unaccounted for variables for prediction

    extra_vars <- setdiff(all_vars, unique(df_effects$marginal))

    if(length(extra_vars)!=0 & is.null(pred_vars)){

      pred_vars <- as.data.frame(matrix(0, 1, length(extra_vars)))

      colnames(pred_vars) <- extra_vars
    }

    if(length(extra_vars)!=0)
      df_pred <- cbind(df_pred, pred_vars)

    # are there fixed effects? If yes takes average across all fixef levels

    if(!is.null(fixef)){

      if(class(unlist(fixef))!="factor") stop("Please supply fixed effects as factors!")

      # convert into dataframe and factor

      fixef <- expand.grid(fixef)

      preds <-
        callapply("rbind", 1:nrow(fixef), function(x){

          fixef_df <- as.data.frame(fixef[x,])

          colnames(fixef_df) <- names(fixef)

          predict(mod, cbind(df_pred, fixef_df))
        })

      preds <- apply(preds, 2, mean)

    } else preds <- predict(mod, df_pred)

    df_pred <- data.frame(all_pred,
                          estimate = preds, std.error = NA, p.value = NA)

    df_pred <- transform(df_pred, value = "Level")

    df_all <- rbind(cond_effs, df_pred)

  } else df_all <- cond_effs

  # rename variables if specified

  if(!is.null(.names)){

    for (i in .names)
      colnames(df_all) <- gsub(i, setNames(names(.names), .names)[i], colnames(df_all))
  }

  return(df_all)
}
