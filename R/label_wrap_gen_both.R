#' @title Wrap labels
#' @description Wrapper around ggplot's label_both function. Wraps and retains variable names in a facet label
#'
#' @param width Maximum number of characters before wrapping the strip.
#' @param multi_line Whether to display the labels of multiple factors on separate lines.
#' @param sep String separating variables and values.
#' @importFrom ggplot2 label_value


label_wrap_gen_both <- function(width = 10, multi_line = TRUE, sep = ": ") {
  fun <- function(labels) {
    value <- label_value(labels, multi_line = multi_line)

    if (multi_line) {
      row <- as.list(names(labels))
    }
    else {
      row <- list(paste(names(labels), collapse = ", "))
    }
    variable <- lapply(row, rep)

    if (multi_line) {
      out <- vector("list", length(value))
      for (i in seq_along(out)) {
        out[[i]] <- paste(variable[[i]], value[[i]], sep = sep)
      }
    }
    else {
      value <- inject(paste(!!!value, sep = ", "))
      variable <- inject(paste(!!!variable, sep = ", "))
      out <- Map(paste, variable, value, sep = sep)
      out <- list(unname(unlist(out)))
    }
    lapply(out, function(st)
      vapply(strwrap(st, width = width, simplify = FALSE), paste, character(1), collapse = "\n"))
  }
  structure(fun, class = "labeller")
}
