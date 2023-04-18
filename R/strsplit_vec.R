#' @title unlist split strings
#' @description Wrapper around `strsplit`


strsplit_vec <- function(x,
                         split,
                         ...) {
  unlist(strsplit(x, split, ...))

}
