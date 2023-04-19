
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

mod <- lm(Y~X1*X2*X3, toydata)
dat <- int_conditions(mod, toydata)
dat |> head()
#>       X1  X2  X3    estimate  std.error   p.value         value
#> 1 effect   0   0 -0.26297759 0.18733435 0.1606951 Causal effect
#> 2 effect all   0 -0.17722875 0.13035876 0.1742825 Causal effect
#> 3 effect   1   0 -0.09009687 0.18121544 0.6191716 Causal effect
#> 4 effect   0 all -0.17663386 0.12747648 0.1661747 Causal effect
#> 5 effect   0   1 -0.09629085 0.17371341 0.5794926 Causal effect
#> 6 effect all all -0.03022122 0.09054353 0.7386195 Causal effect
```

We can then graph the result:
