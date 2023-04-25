#' @title Check vectors are mutually in each other
#' @description NA
#'
#' @param terms_vec vector of all terms to be split
#' @param vars a vector of variables of a term

ind_fn <-
  function(terms_vec, vars){
    sapply(terms_vec, function(z){
      vec <- strsplit_vec(z, ":")
      all(c(all(vec %in% vars), all(vars %in% vec)))
    })
  }
