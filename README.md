
# Rinteract

[![R-CMD-check](https://github.com/jonfoong/Rinteract/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jonfoong/Rinteract/actions/workflows/R-CMD-check.yaml)[![codecov](https://codecov.io/github/jonfoong/Rinteract/branch/main/graph/badge.svg?token=2SOK4T1220)](https://codecov.io/github/jonfoong/Rinteract)[![CRAN
status](https://www.r-pkg.org/badges/version/Rinteract.png)](https://CRAN.R-project.org/package=Rinteract)[![License:
GPL-3](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://cran.r-project.org/web/licenses/GPL-3)[![](https://img.shields.io/badge/devel%20version-0.1.0-blue.svg)](https://github.com/jonfoong/Rinteract)

Rinteract is a small package that makes it easy to output all
conditional effects in models with interaction terms instead of
computing hypothesis tests manually.

## Installation

``` r

devtools::install_github("jonfoong/Rinteract")
```

## Overview

Interaction terms are widely used in regression models to uncover
underlying heterogeneity for a predictor of interest. Despite its
ubiquity, interpretation of interaction models is often confusing at
best and inaccurate at worst. In addition, studies with interaction
terms typically only report estimates from a model output. As pointed
out by Brambor et al (2006):

> …typical results table often conveys very little information of
> interest because the analyst is not concerned with model parameters
> per se; he or she is primarily interested in the marginal effect of X
> on Y for substantively meaningful values of the conditioning variable
> Z. While it is often possible to calculate the marginal effect of X
> for any value of Z from the typical results table using a little
> algebra, the problem is that it is not possible to do the same for the
> standard errors.

`Rinteract` avoids this by computing all conditional effects in a model
with interactions. It accepts a model object as input and relies on
`multcomp` to perform hypothesis testing on all conditions of interest
(when a variable is at 0, 1, or the mean). It is also capable of
graphing these effects using tabular ggplots.

Note: As of now the package only accepts `lm`, `glm` and `lm_robust`
models.

## Usage

``` r
library(Rinteract)
library(kableExtra)

mod <- lm(Y~X1*X2*X3, toydata)
dat <- int_conditions(mod, toydata)
dat |> 
  head() |>
  kable(digits = 3)
```

<table>
 <thead>
  <tr>
   <th style="text-align:left;"> X1 </th>
   <th style="text-align:left;"> X2 </th>
   <th style="text-align:left;"> X3 </th>
   <th style="text-align:right;"> estimate </th>
   <th style="text-align:right;"> std.error </th>
   <th style="text-align:right;"> p.value </th>
   <th style="text-align:left;"> value </th>
  </tr>
 </thead>
<tbody>
  <tr>
   <td style="text-align:left;"> effect </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> -0.263 </td>
   <td style="text-align:right;"> 0.187 </td>
   <td style="text-align:right;"> 0.161 </td>
   <td style="text-align:left;"> Causal effect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> effect </td>
   <td style="text-align:left;"> all </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> -0.177 </td>
   <td style="text-align:right;"> 0.130 </td>
   <td style="text-align:right;"> 0.174 </td>
   <td style="text-align:left;"> Causal effect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> effect </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:right;"> -0.090 </td>
   <td style="text-align:right;"> 0.181 </td>
   <td style="text-align:right;"> 0.619 </td>
   <td style="text-align:left;"> Causal effect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> effect </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> all </td>
   <td style="text-align:right;"> -0.177 </td>
   <td style="text-align:right;"> 0.127 </td>
   <td style="text-align:right;"> 0.166 </td>
   <td style="text-align:left;"> Causal effect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> effect </td>
   <td style="text-align:left;"> 0 </td>
   <td style="text-align:left;"> 1 </td>
   <td style="text-align:right;"> -0.096 </td>
   <td style="text-align:right;"> 0.174 </td>
   <td style="text-align:right;"> 0.579 </td>
   <td style="text-align:left;"> Causal effect </td>
  </tr>
  <tr>
   <td style="text-align:left;"> effect </td>
   <td style="text-align:left;"> all </td>
   <td style="text-align:left;"> all </td>
   <td style="text-align:right;"> -0.030 </td>
   <td style="text-align:right;"> 0.091 </td>
   <td style="text-align:right;"> 0.739 </td>
   <td style="text-align:left;"> Causal effect </td>
  </tr>
</tbody>
</table>

We can then graph the result:

``` r
library(ggplot2)

int_graph(dat, X1~X2+X3) +
  ggtitle("all cond effects")
```

![](vignettes/vignette-unnamed-chunk-3-1.png)
