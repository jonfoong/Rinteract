#' @title unlist split strings
#' @description Wrapper around `strsplit`
#'
#' @param x A string to be split.
#' @param split A regexp for the string to be split by.
#' @param ... Additional arguments passed to `strsplit`


strsplit_vec <- function(x, split, ...) {
  unlist(strsplit(x, split, ...))
}
