% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/int_graph.R
\name{int_graph}
\alias{int_graph}
\title{Graph conditional means and effects from a regression with interactions}
\usage{
int_graph(
  data,
  facet = NULL,
  digits = 3,
  width = 10,
  col_effect = c("#FBB4AE", "#B3CDE3", "#CCEBC5", "#DECBE4"),
  col_level = "#F0F0F0",
  col_label = "#DEEBF7",
  eff_var = NULL,
  alpha_e = 1,
  alpha_l = 1,
  col_values = c("black", "red"),
  label_vals = NULL,
  lab_both = TRUE
)
}
\arguments{
\item{data}{output of a `int_conditions` call}

\item{facet}{A formula of the form X~Y with all interaction terms specified. Must be supplied.}

\item{digits}{How many decimal digits should be displayed?}

\item{width}{Maximum number of characters before wrapping the strip.}

\item{col_effect}{Colour of background panel for each effect type. The default colors are taken from `RColorBrewer`'s Pastel1 palette. Length of vector must be either 1 (in which case all faceted effects have the same color) or n, where n is the number of faceted variables in the dataset. By default, all positive values are black and negative ones red.}

\item{col_level}{Colour of background panel for all levels.}

\item{col_label}{Background colour of label strip.}

\item{eff_var}{Which variable should the color for the effect panel be faceted on? Defaults to the variable used for the outer facet.}

\item{alpha_e}{The alpha level for the effect panels. Defaults to 1.}

\item{alpha_l}{The alpha level for the level panels. Defaults to 1.}

\item{col_values}{What colour should values take? Takes a vector up to length 2 for positive and negative values, or length one, in which case all values have the same colour.}

\item{label_vals}{By default, `int_graph` takes label values supplied from the output of `int_conditions`. To change these values, supply a list of named vectors.}

\item{lab_both}{Logical. Should `int_graph` show both names and values of labels?}
}
\value{
A ggplot object that plots all conditional means and effects from the output of `int_conditions`. Values in red are negative estimates while bold represents estimates with p<0.05.
}
\description{
Wrapper around ggplot that graphs all conditional means and effects from the output of `int_conditions`
}
\examples{
library(ggplot2)

set.seed(1)
dat <- data.frame(X1 = sample(0:1, 100, replace=TRUE), X2 = sample(0:1, 100, replace=TRUE), X3 = sample(0:1, 100, replace=TRUE))
dat <- dat |> transform(Y = X1 + 2*X2 + 3*X1*X2*X3 + rnorm(1))
mod <- lm(Y~X1*X2*X3, dat)
cond_tab <- int_conditions(mod, data = dat, .names = c(A1 = "X1", A2 = "X2", A3 = "X3"))
plot <- int_graph(cond_tab, facet = A1~A2+A3)

# Output is a ggplot object that can be manually manipulated further
plot + ggtitle("testing")

}
