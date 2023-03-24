
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clptheory

<!-- badges: start -->
<!-- badges: end -->

The goal of clptheory (*classical price theory*) is to create a suite of
functions to compute the uniform rate of profit, the vector of price of
production and the vector of labor values for different specifications
of the circulating capital model and the capital stock model.

## Installation

You can install the development version of clptheory from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dbasu-umass/clptheory")
```

## Example

This is a basic example which shows you how to compute the uniform rate
of profit and the vectors of labor values and prices of production for a
basic circulating capital model:

``` r
library(clptheory)

# ------ Data

# Input-output matrix
A <- matrix(
  data = c(0.265,0.968,0.00681,0.0121,0.391,0.0169,0.0408,0.808,0.165),
  nrow=3, ncol=3, byrow = TRUE
)

# Direct labor input vector
l <- matrix(
  data = c(0.193, 3.562, 0.616),
  nrow=1
)

# Real wage bundle vector
b <- matrix(
  data = c(0.0109, 0.0275, 0.296),
  ncol=1
)

# Gross output vector
Q <- matrix(
  data = c(26530, 18168, 73840),
  ncol=1
)

# Estimate circulating capital model with SI
# and see results on screen
ppstdint1(
  A = A,
  l = l,
  b = b,
  Q = Q,
  l_simple = l
)
#> $meig
#> [1] 0.7205731
#> 
#> $urop
#> [1] 0.3877843
#> 
#> $mrop
#> [1] 1.024939
#> 
#> $ppabs
#>           [,1]      [,2]     [,3]
#> [1,] 0.5703988 0.2388832 1.341621
#> 
#> $pprel
#> [1] 0.3861133 0.1617044 0.9081675
#> 
#> $lvalues
#>           [,1]     [,2]      [,3]
#> [1,] 0.4398417 7.739431 0.8979541
#> 
#> $dprice
#>          [,1]     [,2]      [,3]
#> [1,] 0.238526 4.197091 0.4869603
#> 
#> $mevg
#> [1] 0.5422997
#> 
#> $mnonneg
#> [1] 1
#> 
#> $mirred
#> [1] 1
```
