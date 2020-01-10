
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ezcox: Easily Process a Batch of Cox Models

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/ezcox)](https://CRAN.R-project.org/package=ezcox)
[![](http://cranlogs.r-pkg.org/badges/grand-total/ezcox?color=blue)](https://cran.r-project.org/package=ezcox)
[![HitCount](http://hits.dwyl.io/ShixiangWang/ezcox.svg)](http://hits.dwyl.io/ShixiangWang/ezcox)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/ShixiangWang/ezcox?branch=master&svg=true)](https://ci.appveyor.com/project/ShixiangWang/ezcox)
[![Travis build
status](https://travis-ci.org/ShixiangWang/ezcox.svg?branch=master)](https://travis-ci.org/ShixiangWang/ezcox)
[![Codecov test
coverage](https://codecov.io/gh/ShixiangWang/ezcox/branch/master/graph/badge.svg)](https://codecov.io/gh/ShixiangWang/ezcox?branch=master)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
<!-- badges: end -->

The goal of ezcox is to operate a batch of univariate or multivariate
Cox models and return tidy result.

## Installation

You can install the released version of ezcox from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("ezcox")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("remotes")
remotes::install_github("ShixiangWang/ezcox")
```

Visualization feature of **ezcox** needs the recent version of
**forestmodel**, please run the following commands:

``` r
remotes::install_github("ShixiangWang/forestmodel")
```

## Example

This is a basic example which shows you how to get result from a batch
of cox models.

``` r
library(ezcox)
#> Welcome to 'ezcox' package!
#> =======================================================================
#> You are using ezcox version 0.4.0
#> 
#> Github page  : https://github.com/ShixiangWang/ezcox
#> Documentation: https://shixiangwang.github.io/ezcox/articles/ezcox.html
#> 
#> Run citation("ezcox") to see how to cite 'ezcox'.
#> =======================================================================
#> 
data("lung", package = "survival")

# Build unvariable models
ezcox(lung, covariates = c("age", "sex", "ph.ecog"))
#> => Processing variable age
#> ==> Building Surv object...
#> ==> Building Cox model...
#> ==> Done.
#> => Processing variable sex
#> ==> Building Surv object...
#> ==> Building Cox model...
#> ==> Done.
#> => Processing variable ph.ecog
#> ==> Building Surv object...
#> ==> Building Cox model...
#> ==> Done.
#> # A tibble: 3 x 12
#>   Variable is_control contrast_level ref_level n_contrast n_ref    beta    HR
#>   <chr>    <lgl>      <chr>          <chr>          <int> <int>   <dbl> <dbl>
#> 1 age      FALSE      age            age              228   228  0.0187 1.02 
#> 2 sex      FALSE      sex            sex              228   228 -0.531  0.588
#> 3 ph.ecog  FALSE      ph.ecog        ph.ecog          227   227  0.476  1.61 
#> # … with 4 more variables: lower_95 <dbl>, upper_95 <dbl>, p.value <dbl>,
#> #   global.pval <dbl>

# Build multi-variable models
# Control variable 'age'
ezcox(lung, covariates = c("sex", "ph.ecog"), controls = "age")
#> => Processing variable sex
#> ==> Building Surv object...
#> ==> Building Cox model...
#> ==> Done.
#> => Processing variable ph.ecog
#> ==> Building Surv object...
#> ==> Building Cox model...
#> ==> Done.
#> # A tibble: 4 x 12
#>   Variable is_control contrast_level ref_level n_contrast n_ref    beta    HR
#>   <chr>    <lgl>      <chr>          <chr>          <int> <int>   <dbl> <dbl>
#> 1 sex      FALSE      sex            sex              228   228 -0.513  0.599
#> 2 sex      TRUE       age            age              228   228  0.017  1.02 
#> 3 ph.ecog  FALSE      ph.ecog        ph.ecog          227   227  0.443  1.56 
#> 4 ph.ecog  TRUE       age            age              228   228  0.0113 1.01 
#> # … with 4 more variables: lower_95 <dbl>, upper_95 <dbl>, p.value <dbl>,
#> #   global.pval <dbl>
```

## Citation

  - Wang, Shixiang, et al. “The predictive power of tumor mutational
    burden in lung cancer immunotherapy response is influenced by
    patients’ sex.” International journal of cancer (2019).
