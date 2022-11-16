
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vectorizedCorrelations

<!-- badges: start -->

[![R-CMD-check](https://github.com/abner-hb/vectorizedCorrelations/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/abner-hb/vectorizedCorrelations/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/abner-hb/vectorizedCorrelations/branch/main/graph/badge.svg)](https://app.codecov.io/gh/abner-hb/vectorizedCorrelations?branch=main)
<!-- badges: end -->

The goal of vectorizedCorrelations is to compute correlations using only
basic R operations and vectorization.

## Installation

You can install the development version of vectorizedCorrelations from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("abner-hb/vectorizedCorrelations")
```

## Example

``` r
library(vectorizedCorrelations)
library(datasets)

A = cars[, 1]
B = cars[, 2]
# Pearson correlation
matrix_cor(A, B, method = "pearson")
#> [1] 0.8068949
# Kendall correlation
matrix_cor(A, B, method = "kendall")
#> [1] 0.7093878
```
