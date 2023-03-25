
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

## Example of Circulating Capital Model

### Standard Interpretation

This is a basic example which shows you how to compute the uniform rate
of profit and the vectors of labor values and prices of production for a
basic circulating capital model using the Standard Interpretation:

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

# Market prices vector
m <- matrix(
  data = c(4, 60, 7),
  nrow=1
)

# Uniform nominal wage rate
wavg <- m%*%b

# Vector of nominal wage rates
# (For simplicity, we use the uniform wage rate)
w <- matrix(
  data = rep(wavg,3),
  nrow = 1
)

# Estimate circulating capital model with SI
si1 <- ppstdint1(
  A = A,
  l = l,
  b = b,
  Q = Q,
  l_simple = l
)
```

What is the uniform rate of profit?

``` r
si1$urop
#> [1] 0.3877843
```

What is the vector of labor values?

``` r
si1$lvalues
#>           [,1]     [,2]      [,3]
#> [1,] 0.4398417 7.739431 0.8979541
```

What is the vector of prices of production (absolute)?

``` r
si1$ppabs
#>           [,1]      [,2]     [,3]
#> [1,] 0.5703988 0.2388832 1.341621
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
nregtestrel(
  x = si1$ppabs,
  y = si1$lvalues,
  w = w,
  w_avg = wavg[1,1],
  mev = si1$mevg,
  Q = Q
)
#> $rmse
#> [1] 23.68697
#> 
#> $mad
#> [1] 14.04216
#> 
#> $mawd
#> [1] 1.51884
#> 
#> $cdm
#> [1] 1.51884
#> 
#> $angle
#> [1] 59.21479
#> 
#> $distangle
#> [1] 0.9881081
```

### New Interpretation

This is a basic example which shows you how to compute the uniform rate
of profit and the vectors of labor values and prices of production for a
basic circulating capital model using the New Interpretation:

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

# Value of labor power
v <- 2/3

# Estimate circulating capital model with NI
ni1 <- ppnewint1(
  A = A,
  l = l,
  w = wavg[1,1],
  v = v,
  Q = Q,
  l_simple = l
)
```

What is the uniform rate of profit?

``` r
ni1$urop
#> [1] 0.2116339
```

What is the vector of labor values?

``` r
ni1$lvalues
#>           [,1]     [,2]      [,3]
#> [1,] 0.4398417 7.739431 0.8979541
```

What is the vector of prices of production (absolute)?

``` r
ni1$ppabs
#>          [,1]     [,2]     [,3]
#> [1,] 2.621517 45.47507 4.703682
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
nregtestrel(
  x=ni1$ppabs,
  y=ni1$lvalues,
  w=w,
  w_avg=wavg[1,1],
  mev=ni1$mevg,
  Q=Q
  )
#> $rmse
#> [1] 0.1064786
#> 
#> $mad
#> [1] 0.09129555
#> 
#> $mawd
#> [1] 0.03132663
#> 
#> $cdm
#> [1] 0.03132663
#> 
#> $angle
#> [1] 3.519195
#> 
#> $distangle
#> [1] 0.06141188
```
