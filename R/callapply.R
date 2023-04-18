#' @title specifying function call to lapply output
#' @description Wrapper around `do.call` and `lapply`


callapply <-
  function(func, x, FUN){
    df <- do.call(func, lapply(x, FUN))
    as.data.frame(df)
  }
