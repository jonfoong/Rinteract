#' @title int_conditions
#'
#'
#' @description Conducts hypothesis testing across all conditions of interaction effects of a fitted model
#'
#' @param mod A model object
#' @param main_vars A vector of variable names in the interaction of interest. If unspecified, takes by default variables from the highest order interaction
#' @param demean Should the function also return mean conditions?
#' @param data Dataset used when fitting model
#' @param names A named vector for renaming variables
#'
#' @return A data frame object that contains all effects, respective conditions, and estimated hypotheses
#' @examples
#' set.seed(1)
#' dat <- data.frame(X1 = sample(0:1, 100, replace=TRUE), X2 = sample(0:1, 100, replace=TRUE))
#' dat <- dat %>% mutate(Y = X1 + 2*X2 + 3*X1*X2)
#' mod <- lm(Y~X1*X2, dat)
#' cond_tab <- int_conditions(mod, main_vars = c("X1", "X2"), data = dat, names = c(A1 = "X1", A2 = "X2"))
#'
#' @export
#' @importFrom dplyr %>% select mutate rowwise across if_all select_if bind_rows rename_with
#' @importFrom tidyr separate
#' @importFrom tidyselect all_of everything
#' @importFrom stringr str_replace_all
#' @importFrom multcomp glht
#' @importFrom rlang is_empty
#' @importFrom broom tidy


int_conditions <- function(mod,
                           main_vars = NULL,
                           demean = TRUE, # if TRUE, returns all demeaned effects
                           data = NULL,
                           names = NULL # takes a named vector; if specified, renames variables of model
){

  # extract all vars from model

  all_vars <-
    attributes(mod$terms)$term.labels

  # if main_vars not specified, extracts highest order interaction

  if(is.null(main_vars)){

    full_term <-
      all_vars[sapply(all_vars,
                      function(x){

                        len <-
                          strsplit(x, ":") %>%
                          unlist() %>%
                          length()

                      }) %>%
                 which.max()]

    main_vars <-
      full_term %>%
      strsplit(":") %>%
      unlist()

  } else {

    # extract the full x-way interaction term which we use later

    full_term <-
      paste(main_vars, collapse = ":")

  }

  # how many terms?

  interacts <- length(main_vars)

  # exclude those we are not conditioning on

  vars <-
    all_vars[all_vars %>%
               sapply(function(x) all((strsplit(x, ":") %>% unlist()) %in% main_vars))]


  # get each interaction effect and the conditions they are marginal to

  margins_df <-
    suppressWarnings(data.frame(marginal = vars) %>%
                       separate(marginal, into = LETTERS[1:interacts],
                                sep = ":", remove = FALSE))

  df <-
    lapply(vars, function(x){

      marginal <-
        margins_df %>%
        rowwise() %>%
        filter(all(strsplit(x, ":") %>%
                     unlist() %in% eval(parse(text = paste0("c(", paste(LETTERS[1:interacts], collapse = ","), ")"))))) %>%
        .$marginal

      data.frame(marginal = marginal,
                 term = x)
    }) %>%
    bind_rows()

  # create a dataframe with all values as variable names

  mat <-
    vars %>%
    as.data.frame() %>%
    t() %>%
    .[rep(1, nrow(df)), ]

  colnames(mat) <- vars
  rownames(mat) <- NULL

  # now combine

  df <-
    cbind(df, mat)

  # create a sparse matrix of 0s and 1s to feed into our hypothesis testing

  df <-
    df %>% rowwise() %>%
    mutate(across(all_of(vars),
                  function(x) ifelse(all((strsplit(x, ":") %>%
                                            unlist) %in% (strsplit(marginal, ":") %>% unlist)) & all((strsplit(term, ":") %>% unlist) %in% (strsplit(x, ":") %>% unlist)), 1, 0))) %>%
    mutate(intercept = 0, .after = term)

  # now extract coeffs and perform hypothesis testing

  cond_effs <-
    lapply(1:nrow(df),
           function(x){

             dat <- df[x,]

             # extract all marginal terms

             form_terms <-
               dat %>%
               select_if(function(x) x==1) %>%
               colnames()

             # create formula

             form <- paste(form_terms, collapse = "+")

             #extract all marginal terms in the formula

             main_terms <-
               dat$term %>%
               strsplit(":") %>%
               unlist()

             marg_terms <-
               form_terms %>%
               strsplit(":") %>%
               unlist() %>%
               unique() %>%
               setdiff(main_terms)

             #create a grid of main terms first

             grid <-
               matrix(1, 1, length(main_terms)) %>%
               as.data.frame()
             colnames(grid) <- main_terms


             # if there are marginal conditions, create matrix of permutations and add back into grid

             if(!is_empty(marg_terms)){

               grid <-
                 eval(parse(text = paste0("expand.grid(",
                                          paste0(marg_terms, "=c(0.5,1)") %>%
                                            paste(collapse = ","), ")"))) %>%
                 cbind(grid)

             }

             if(!demean){

               grid <- grid %>%
                 filter(if_all(everything(), function(x) x==1))

             } else {

               # if demean, replace 0.5 with means

               grid <-
                 grid %>%
                 mutate(across(everything(),
                               function(x){
                                 m <-
                                   data[[cur_column()]] %>%
                                   mean(na.rm = TRUE)

                                 ifelse(x==0.5, m, x)

                               }))
             }

             # now create df of all hypotheses we have to test incl formula

             hyp_tests <-
               grid %>%
               mutate(form = form) %>%
               separate(form, into = paste0("t_", 1:length(form_terms)),
                        sep = "\\+") %>%
               mutate(across(starts_with("t"), function(x){

                 form <- str_replace_all(x, "\\:", "*")
                 num <- eval(parse(text = form))
                 sprintf("%f*%s", num, x)
               })) %>%
               rowwise() %>%
               mutate(form = paste0(paste(across(starts_with("t_")),
                                          collapse = "+"), "=0"))

             # now we run hypotheses one by one

             all_hyps <-
               lapply(1:nrow(hyp_tests),
                      function(x){

                        dat_hyp <- hyp_tests[x,]


                        #run linear hypothesis using formula
                        test <-
                          glht(mod, dat_hyp$form) %>%
                          tidy() %>%
                          select(estimate, std.error, `p.value`=adj.p.value)

                        # extract all conditions

                        # get conditions for when x=1 or mean

                        con_mean <-
                          c(dat_hyp %>%
                              select(!matches(sprintf("t_\\d|form|%s", dat$term))) %>%
                              mutate(across(everything(),
                                            function(x) paste0(cur_column(), " = ",
                                                               ifelse(!x %in% 0:1, "'mean'", x)))) %>%
                              unlist(),
                            dat$term %>% strsplit(":") %>% unlist() %>% paste0(" = 1")
                          ) %>%
                          unique()

                        con_mean <-
                          eval(parse(text = paste0("data.frame(",
                                                   paste(con_mean, collapse=","), ")")))
                        # get all zero conditions

                        zero_names <-
                          main_vars %>%
                          setdiff(names(con_mean))

                        con_zero <-
                          matrix(0, 1, length(zero_names))

                        colnames(con_zero) <- zero_names

                        conditions <-
                          cbind(con_mean, con_zero)

                        # build dataframe

                        new_df <-
                          conditions %>%
                          mutate(effect = dat$term, .before = 1) %>%
                          mutate(across(everything(), function(x) as.character(x))) %>%
                          cbind(test)

                      }) %>%
               bind_rows()

           }) %>%
    bind_rows()

  # rename variables if specified

  if(!is.null(names)){

    cond_effs <-
      cond_effs %>%
      mutate(effect = str_replace_all(effect,
                                      setNames(names(names), names))) %>%
      rename_with(.cols = all_of(main_vars),
                  .fn = function(x) str_replace_all(x, setNames(names(names), names)))

  }

  cond_effs

}
