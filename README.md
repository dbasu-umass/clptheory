
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clptheory

<!-- badges: start -->
<!-- badges: end -->

The goal of clptheory (*classical price theory*) is to create a suite of
functions to implement the classical theory of prices. The functions in
this package computes the uniform rate of profit, the vector of price of
production and the vector of labor values for different specifications
of the circulating capital model and the capital stock model. The
functions also computes various regression- and non-regression-based
measures of deviation between the vector of *all possible* relative
prices of production and the vector of *all possible* relative labor
values.

## Installation

You can install the development version of clptheory from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("dbasu-umass/clptheory")
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

## Examples

These are examples of a 3-industry economy which shows you how to:

1.  compute the uniform rate of profit and the vectors of labor values
    and prices of production for a basic circulating capital model using
    the Standard Interpretation; and

2.  compute regression- and non-regression-based measures of deviation
    between the vector of *all possible* relative prices of production
    and the vector of *all possible* relative labor values.

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
# Load library
library(clptheory)

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
(nrsi1)
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
#> 
#> $lrelpplv
#> [1] 3
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
rsi1 <- regtestrel(x=si1$ppabs,y=si1$lvalues)
(rsi1)
#> $a0lg
#> (Intercept) 
#>  -0.8107143 
#> 
#> $a1lg
#>  log(relv) 
#> -0.5054946 
#> 
#> $r2lg
#> [1] 0.9289345
#> 
#> $fstatlg
#> [1] 58.02628
#> 
#> $pvallg
#> [1] 0.0924
#> 
#> $nlg
#> [1] 3
#> 
#> $a0lv
#> (Intercept) 
#>    1.473692 
#> 
#> $a1lv
#>       relv 
#> -0.1560277 
#> 
#> $r2lv
#> [1] 0.3863259
#> 
#> $fstatlv
#> [1] 20.81069
#> 
#> $pvallv
#> [1] 0.1532
#> 
#> $nlv
#> [1] 3
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
(nrsi2)
#> $rmse
#> [1] 1.152957
#> 
#> $mad
#> [1] 1.031963
#> 
#> $mawd
#> [1] 0.2125032
#> 
#> $cdm
#> [1] 0.2125032
#> 
#> $angle
#> [1] 51.23735
#> 
#> $distangle
#> [1] 0.8647594
#> 
#> $lrelpplv
#> [1] 3
```

Finally, let us conduct regression-based tests for deviation.

``` r
rsi2 <- regtestrel(x=si2$ppabs,y=si2$lvalues)
(rsi2)
#> $a0lg
#> (Intercept) 
#>  -0.7206045 
#> 
#> $a1lg
#> log(relv) 
#> 0.4495966 
#> 
#> $r2lg
#> [1] 0.9271353
#> 
#> $fstatlg
#> [1] 11.47059
#> 
#> $pvallg
#> [1] 0.2044
#> 
#> $nlg
#> [1] 3
#> 
#> $a0lv
#> (Intercept) 
#>   0.1682603 
#> 
#> $a1lv
#>      relv 
#> 0.1528502 
#> 
#> $r2lv
#> [1] 0.9999158
#> 
#> $fstatlv
#> [1] 280309.6
#> 
#> $pvallv
#> [1] 0.0013
#> 
#> $nlv
#> [1] 3
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
(nrni1)
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
#> 
#> $lrelpplv
#> [1] 3
```

Let us now compute the various regression-based measures of deviation
between the vector of all possible relative labor values and the vector
of all possible relative prices of production.

``` r
rni1 <- regtestrel(x=ni1$ppabs,y=ni1$lvalues)
(rni1)
#> $a0lg
#> (Intercept) 
#>  0.09496661 
#> 
#> $a1lg
#> log(relv) 
#>  1.018689 
#> 
#> $r2lg
#> [1] 0.9997416
#> 
#> $fstatlg
#> [1] 3.915071
#> 
#> $pvallg
#> [1] 0.3365
#> 
#> $nlg
#> [1] 3
#> 
#> $a0lv
#>  (Intercept) 
#> 0.0007242004 
#> 
#> $a1lv
#>     relv 
#> 1.121668 
#> 
#> $r2lv
#> [1] 0.9999983
#> 
#> $fstatlv
#> [1] 5618.79
#> 
#> $pvallv
#> [1] 0.0094
#> 
#> $nlv
#> [1] 3
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
(nrni2)
#> $rmse
#> [1] 0.295736
#> 
#> $mad
#> [1] 0.2646106
#> 
#> $mawd
#> [1] 0.07588155
#> 
#> $cdm
#> [1] 0.07588155
#> 
#> $angle
#> [1] 7.288633
#> 
#> $distangle
#> [1] 0.1271249
#> 
#> $lrelpplv
#> [1] 3
```

Let us now compute the various regression-based measures of deviation
between the vector of all possible relative labor values and the vector
of all possible relative prices of production.

``` r
rni2 <- regtestrel(x=ni2$ppabs,y=ni2$lvalues)
(rni1)
#> $a0lg
#> (Intercept) 
#>  0.09496661 
#> 
#> $a1lg
#> log(relv) 
#>  1.018689 
#> 
#> $r2lg
#> [1] 0.9997416
#> 
#> $fstatlg
#> [1] 3.915071
#> 
#> $pvallg
#> [1] 0.3365
#> 
#> $nlg
#> [1] 3
#> 
#> $a0lv
#>  (Intercept) 
#> 0.0007242004 
#> 
#> $a1lv
#>     relv 
#> 1.121668 
#> 
#> $r2lv
#> [1] 0.9999983
#> 
#> $fstatlv
#> [1] 5618.79
#> 
#> $pvallv
#> [1] 0.0094
#> 
#> $nlv
#> [1] 3
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

## Analysis for Australia

Let us conduct price of production analysis for Australia (AUS).

### Data

Let us create the data objects.

``` r
ausdata <- createdata(
  country = "AUS", year = 2010, 
  datasea = aussea, dataio = ausiot
  )
#> c("C33", "M71", "M72", "M73", "M74_M75", "U")
```

### Standard Interpretation

Let us now estimate the circulating capital model with SI.

``` r
siaus <- ppstdint1(
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
niaus <- ppnewint1(
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
cbind(siaus$urop,niaus$urop)
#>           [,1]     [,2]
#> [1,] 0.6018444 0.973433
```

### Measures of Deviation

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
#> rmse      21.0267   0.08327901
#> mad       10.2423   0.0589563 
#> mawd      5.531983  0.0220081 
#> cdm       6.010217  0.2442277 
#> angle     59.97441  4.712209  
#> distangle 0.9996131 0.08222038
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
