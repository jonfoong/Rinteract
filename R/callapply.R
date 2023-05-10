#' @title specifying function call to lapply output
#' @description Wrapper around `do.call` and `lapply`
#'
#' @param func A string for a function call to an `lapply` output. Common uses would be "rbind" or "cbind"
#' @param x A vector to iterate over for `lapply`
#' @param FUN A function supplied to `lapply`
#' @param ... Other arguments passed to `lapply`


callapply <-
  function(func, x, FUN, ...){
    df <- do.call(func, lapply(x, FUN, ...))
    as.data.frame(df)
  }
