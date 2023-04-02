
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clptheory

<!-- badges: start -->
<!-- badges: end -->

The goal of `clptheory` (*classical price theory*) is to create a suite
of functions to implement the classical theory of prices. The functions
in this package computes the uniform rate of profit, the vector of price
of production and the vector of labor values for different
specifications of the circulating capital model and the capital stock
model. The functions also computes various regression- and
non-regression-based measures of deviation between the vector of *all
possible* relative prices of production and the vector of *all possible*
relative labor values.

## Installation

You can install the package `clptheory` from CRAN with:

``` r
# Uncomment the following line
# install.packages("clptheory")
```

You can install the development version of `clptheory` from
[GitHub](https://github.com/) with:

``` r
# Uncomment the following two lines
# install.packages("devtools")
# devtools::install_github("dbasu-umass/clptheory")
```

## Main Functions

This package provides the following functions.

1.  `ppstdint1`: a function to estimate a basic circulating capital
    model (uniform wage rates across industries + not taking account of
    unproductive industries) with the Standard Interpretation;

2.  `ppstdint2`: a function to estimate a circulating capital model
    (uniform wage rates across industries + takes account of
    unproductive industries) with the Standard Interpretation;

3.  `ppstdint3`: a function to estimate a basic capital stock model
    (uniform wage rates across industries + not taking account of
    unproductive industries) with the Standard Interpretation;

4.  `ppnewint1`: a function to estimate a basic circulating capital
    model (uniform wage rates across industries + not taking account of
    unproductive industries) with the New Interpretation;

5.  `ppnewint2`: a function to estimate a circulating capital model
    (allows differential wage rates across industries + not taking
    account of unproductive industries) with the New Interpretation;

6.  `ppnewint3`: a function to estimate a circulating capital model
    (uniform wage rates across industries + takes account of
    unproductive industries) with the New Interpretation;

7.  `ppnewint4`: a function to estimate a circulating capital model
    (allows differential wage rates across industries + takes account of
    unproductive industries) with the New Interpretation;

8.  `ppnewint5`: a function to estimate a basic capital stock model
    (uniform wage rates across industries + not taking account of
    unproductive industries) with the New Interpretation;

9.  `ppnewint6`: a function to estimate a capital stock model (allows
    differential wage rates across industries + not taking account of
    unproductive industries) with the New Interpretation;

10. `ppnewint7`: a function to estimate a capital stock model (uniform
    wage rates across industries + takes account of unproductive
    industries) with the New Interpretation;

11. `ppnewint8`: a function to estimate a capital stock model (allows
    differential wage rates across industries + takes account of
    unproductive industries) with the New Interpretation;

12. `nregtestrel`: a function that computes various non-regression-based
    measures of deviation between the vector of *relative* prices of
    production and the vector of *relative* labor values;

13. `regtestrel`: a function that computes various regression-based
    measures of deviation between the vector of *relative* prices of
    production and the vector of *relative* labor values;

14. `createdata`: a function to create the data objects (matrices,
    vectors and scalars) necessary to implement the SI and NI.

The package contains the following three datasets.

1.  `aussea`: the socio economic accounts for the Australian economy
    extracted from the 2016 release of the World Input Output Database;
    this data set contains industry-level variables (53 industries) for
    the USA for 15 years, 2000-2014;

2.  `ausiot`: input-output tables for the Australian economy extracted
    from the 2016 release of the World Input Output Database; this data
    set contains 53-industry input-output tables for the USA for 15
    years, 2000-2014;

3.  `usasea`: the socio economic accounts for the USA extracted from the
    2016 release of the World Input Output Database; this data set
    contains industry-level variables (53 industries) for the USA for 15
    years, 2000-2014;

4.  `usaiot`: input-output tables for the USA extracted from the 2016
    release of the World Input Output Database; this data set contains
    53-industry input-output tables for the USA for 15 years, 2000-2014;

5.  `usarwb`: personal consumption expenditure on the output of the 53
    industries of the input-output tables for the USA extracted from the
    2016 release of the World Input Output Database; this data set
    contains data for 15 years, 2000-2014. (Note: This data set is not
    necessary for the analysis.)

## Example 1: Analysis for Australia

Let us conduct price of production analysis for Australia (AUS) and see
how to use the functions in `clptheory` to

1.  compute the uniform rate of profit and the vectors of labor values
    and prices of production for a basic circulating capital model using
    the Standard Interpretation and the New Interpretation; and

2.  compute regression- and non-regression-based measures of deviation
    between the vector of *all possible* relative prices of production
    and the vector of *all possible* relative labor values.

Let us load the package.

``` r
# Load library
library(clptheory)
```

### Data

Let us create the data objects.

``` r
ausdata <- clptheory::createdata(
  country = "AUS", year = 2010, 
  datasea = aussea, dataio = ausiot
  )
#> c("C33", "M71", "M72", "M73", "M74_M75", "U")
```

### Standard Interpretation

Let us now estimate the circulating capital model with SI.

``` r
si1 <- clptheory::ppstdint1(
  A = ausdata$Ahat,
  l = ausdata$l,
  b = ausdata$b,
  Q = ausdata$Q,
  l_simple = ausdata$l_simple
)
```

### New Interpretation

Let us now estimate the circulating capital model with NI.

``` r
ni1 <- clptheory::ppnewint1(
  A = ausdata$Ahat,
  l = ausdata$l,
  w = ausdata$wavg,
  v = ausdata$vlp,
  Q = ausdata$Q,
  l_simple = ausdata$l_simple
)
```

Let us see the uniform profit rate.

``` r
cbind(si1$urop,ni1$urop)
#>           [,1]     [,2]
#> [1,] 0.6018444 0.973433
```

### Non-Regression-Based Measures of Deviation

Let us compute various non-regression-based measures of the deviation
between the vector of *relative* labor values and the vector of
*relative* prices of production for the SI.

``` r
nrsi1 <- clptheory::nregtestrel(
  x = si1$ppabs,
  y = si1$lvalues,
  w = ausdata$wagevector_all,
  w_avg = ausdata$wavg,
  mev = si1$mevg,
  Q = ausdata$Q
)
```

Let us do the same computation for the NI.

``` r
nrni1 <- clptheory::nregtestrel(
  x=ni1$ppabs,
  y=ni1$lvalues,
  w=ausdata$wagevector_all,
  w_avg=ausdata$wavg,
  mev=ni1$mevg,
  Q=ausdata$Q
  )
```

We can now compare the results for the analysis of the *circulating
capital model* from the SI approach and the NI approach for the
non-regression-based measures of deviation between relative prices of
production and relative values.

``` r
comp1 <- cbind(nrsi1,nrni1)
colnames(comp1) <- c("SI","NI")
(comp1)
#>           SI        NI       
#> rmse      3.603886  0.6884111
#> mad       1.60151   0.4600684
#> mawd      0.713937  0.3040913
#> cdm       0.7897359 0.6472872
#> angle     63.92068  23.54431 
#> distangle 1.058664  0.4080406
#> lrelpplv  1128      1128
```

In the results above, we see the magnitudes of six different measures of
the deviation between the vector of *relative* prices of production and
the vector of *relative* labor values: root mean squared error (RMSE),
meann absolute distance (MAD), mean absolute weighted distance (MAWD),
classical distance measure (CDM), angle between the two vectors (angle
in degrees), and distance computed using angle (distance).

As an example, we can see that the CDM for SI is 0.789 and for NI is
0.647. This can be interpreted as showing that the deviation between the
vector of *relative* prices of production and the vector of *relative*
labor values is 79 percent and 65 percent of the relative value vector
according to the SI and NI methodology, respectively.

The last row of the above results shows the length of (number of
observations in) the vector of *relative* prices of production or the
vector of *relative* labor values. Recall that the input-output matrix
is 48 by 48. Hence, the *absolute* value and price of production vectors
will each be of size 48. Thus, the size of the vector of *relative*
prices of production (or labor value) should be $(48 \times 47)/2=1128$.

### Regression-Based Measures of Deviation

Let us compute various regression-based measures of the deviation
between the vector of *relative* labor values and the vector of
*relative* prices of production for the SI.

``` r
rsi1 <- clptheory::regtestrel(
  x = si1$ppabs,
  y = si1$lvalues
)
```

Let us do the same computation for the NI.

``` r
rni1 <- clptheory::regtestrel(
  x=ni1$ppabs,
  y=ni1$lvalues
  )
```

We can now compare the results for the analysis of the *circulating
capital model* from the SI approach and the NI approach for the
regression-based measures of deviation between relative prices of
production and relative values.

``` r
comp2 <- cbind(rsi1,rni1)
colnames(comp2) <- c("SI","NI")
(comp2)
#>         SI          NI       
#> a0lg    -0.7787738  0.1341304
#> a1lg    -0.1450801  0.386964 
#> r2lg    0.001308835 0.1638283
#> fstatlg 143.914     550.9681 
#> pvallg  0           0        
#> nlg     1128        1128     
#> a0lv    1.736712    0.6057521
#> a1lv    -0.3923921  0.5908621
#> r2lv    0.002833572 0.22993  
#> fstatlv 35.7541     266.3688 
#> pvallv  0           0        
#> nlv     1128        1128
```

Regression-based tests of the deviation between use regressions, either
log-log or level-level, of relative prices of production on relative
labor value. The key the null (joint) hypothesis is that the intercept
is 0 and the slope is 1.

The F-stat in the log-log regression of relative prices of production on
relative value is 143.91 for SI and 550.97 for NI. In both cases, we can
strongly reject the null hypothesis that the intercept is 0 and the
slope is 1. The corresponding F-stats for the level-level regressions
are 35.75 (SI) and 266.39 (NI). Once again, the null hypothesis is
strongly rejected.

## Example 2: Simple 3-Industry Set-up

This example was presented on pages 46-57 of E. M. Ochoa’s dissertation
(Ochoa, E. M. 1984. Labor-Value and Prices of Production: An
Interindustry Study of the U.S. Economy, 1947–1972. PhD thesis, *New
School for Social Research*, New York, NY.). This example has also been
discussed in Appendix B of Basu and Moraitis, 2023. (Basu, Deepankar and
Moraitis, Athanasios, “Alternative Approaches to Labor Values and Prices
of Production: Theory and Evidence” (2023). Economics Department Working
Paper Series. 347. *UMass Amherst*. URL:
<https://scholarworks.umass.edu/econ_workingpaper/347/>)

### The Data

Let us load the library and create the data for our examples.

``` r
# Input-output matrix
A <- matrix(
  data = c(0.265,0.968,0.00681,0.0121,0.391,0.0169,0.0408,0.808,0.165),
  nrow=3, ncol=3, byrow = TRUE
)

# Depreciation matrix
D <- matrix(
  data = c(0,0,0,0.00568,0.0267,0.0028,0.00265,0.0147,0.00246),
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

# Capital stock coefficient matrix
K <- matrix(
  data = c(0,0,0,0.120,0.791,0.096,0.037,0.251,0.043),
  nrow=3, ncol=3, byrow = TRUE
)

# Diagonal matrix of turnover times
t <- diag(c(0.317, 0.099, 0.187))

# Uniform nominal wage rate
wavg <- m%*%b

# Vector of nominal wage rates
w <- matrix(
  data = rep(wavg,3),
  nrow = 1
)

# Value of labor power
v <- 2/3
```

### Standard Interpretation

We will first implement the classical theory of prices for the
circulating capital model and then turn to the capital stock model.

#### Circulating Capital Model

``` r

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
nrsi1 <- nregtestrel(
  x = si1$ppabs,
  y = si1$lvalues,
  w = w,
  w_avg = wavg[1,1],
  mev = si1$mevg,
  Q = Q
)
```

#### Capital Stock Model

``` r
# Estimate model
si2 <- ppstdint3(
  A = A,
  l = l,
  b = b,
  Q = Q,
  D = D,
  K = K,
  t = t,
  l_simple = l
)
```

What is the uniform rate of profti?

``` r
si2$urop
#> [1] 0.2337492
```

What is the vector of labor values?

``` r
si2$lvalues
#>           [,1]     [,2]      [,3]
#> [1,] 0.5192079 8.309406 0.9407729
```

What is the vector of prices of production?

``` r
si2$ppabs
#>          [,1]    [,2]     [,3]
#> [1,] 0.284253 1.66129 1.094453
```

Let us now compute the non-regression-based measures of deviation.

``` r
nrsi2 <- nregtestrel(
  x = si2$ppabs,
  y = si2$lvalues,
  w = w,
  w_avg = wavg[1,1],
  mev = si2$mevg,
  Q = Q
)
```

### New Interpretation

We continue working with the 3-industry example and implement the New
Interpretation of Marx’s labor theory of value.

#### Circulating Capital Model

``` r

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
nrni1 <- nregtestrel(
  x=ni1$ppabs,
  y=ni1$lvalues,
  w=w,
  w_avg=wavg[1,1],
  mev=ni1$mevg,
  Q=Q
  )
```

#### Capital Stock Model

``` r
ni2 <- ppnewint5(
  A = A,
  l = l,
  v = v,
  w = wavg[1,1],
  Q = Q,
  D = D,
  K = K,
  t = t,
  l_simple = l
)
```

What is the uniform rate of profit?

``` r
ni2$urop
#> [1] 0.1569311
```

What is the vector of labor values?

``` r
ni2$lvalues
#>           [,1]     [,2]      [,3]
#> [1,] 0.5192079 8.309406 0.9407729
```

What is the vector of prices of production (absolute)?

``` r
ni2$ppabs
#>         [,1]    [,2]     [,3]
#> [1,] 3.90503 48.3282 5.017568
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
nrni2 <- nregtestrel(
  x=ni2$ppabs,
  y=ni2$lvalues,
  w=w,
  w_avg=wavg[1,1],
  mev=ni2$mevg,
  Q=Q
  )
```

## Comparison of SI and NI

We can compare the results for the analysis of the *circulating capital
model* from the SI approach and the NI approach for the
non-regression-based measures of deviation between relative prices of
production and relative values.

``` r
comp1 <- cbind(nrsi1,nrni1)
colnames(comp1) <- c("SI","NI")
(comp1)
#>           SI        NI        
#> rmse      23.68697  0.1064786 
#> mad       14.04216  0.09129555
#> mawd      1.51884   0.03132663
#> cdm       1.51884   0.03132663
#> angle     59.21479  3.519195  
#> distangle 0.9881081 0.06141188
#> lrelpplv  3         3
```

We can compare the results for the analysis of the *capital stock model*
from the SI approach and the NI approach for the non-regression-based
measures of deviation between relative prices of production and relative
values.

``` r
comp2 <- cbind(nrsi2,nrni2)
colnames(comp2) <- c("SI","NI")
(comp2)
#>           SI        NI        
#> rmse      1.152957  0.295736  
#> mad       1.031963  0.2646106 
#> mawd      0.2125032 0.07588155
#> cdm       0.2125032 0.07588155
#> angle     51.23735  7.288633  
#> distangle 0.8647594 0.1271249 
#> lrelpplv  3         3
```

## References

- Basu, D. and Moraitis, T. (2023). Alternative Approaches to Labor
  Values and Prices of Production: Theory and Evidence. *Economics
  Department Working Paper Series*. 347. University of Massachusetts
  Amherst. Retrieved from
  <https://scholarworks.umass.edu/econ_workingpaper/347/>
