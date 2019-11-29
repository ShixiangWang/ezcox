
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
maturing](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)
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
# install.packages("devtools")
devtools::install_github("ShixiangWang/ezcox")
```

## Example

This is a basic example which shows you how to get result from a batch
of cox models.

``` r
library(ezcox)
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
#> # A tibble: 3 x 11
#>   Variable contrast_level ref_level n_contrast n_ref    beta    HR lower_95
#>   <chr>    <chr>          <chr>          <int> <int>   <dbl> <dbl>    <dbl>
#> 1 age      age            age              228   228  0.0187 1.02     1    
#> 2 sex      sex            sex              228   228 -0.531  0.588    0.424
#> 3 ph.ecog  ph.ecog        ph.ecog          227   227  0.476  1.61     1.29 
#> # … with 3 more variables: upper_95 <dbl>, p.value <dbl>, global.pval <dbl>

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
#> # A tibble: 4 x 11
#>   Variable contrast_level ref_level n_contrast n_ref    beta    HR lower_95
#>   <chr>    <chr>          <chr>          <int> <int>   <dbl> <dbl>    <dbl>
#> 1 sex      sex            sex              228   228 -0.513  0.599    0.431
#> 2 sex      age            age              228   228  0.017  1.02     0.999
#> 3 ph.ecog  ph.ecog        ph.ecog          227   227  0.443  1.56     1.24 
#> 4 ph.ecog  age            age              228   228  0.0113 1.01     0.993
#> # … with 3 more variables: upper_95 <dbl>, p.value <dbl>, global.pval <dbl>
```

## Citation

  - Wang, Shixiang, et al. “The predictive power of tumor mutational
    burden in lung cancer immunotherapy response is influenced by
    patients’ sex.” International journal of cancer (2019).
