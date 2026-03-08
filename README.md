
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clptheory

<!-- badges: start -->

<!-- badges: end -->

The goal of `clptheory` (*classical price theory*) is to create a suite
of functions to implement the classical theory of prices. The functions
in this package computes the uniform rate of profit, the vector of price
of production (PP), the vector of direct prices (DP) and the vector of
labor values for the circulating capital model and the capital stock
model. The functions also computes various non-regression-based measures
of deviation between PP/MP, DP/MP and PP/DP, where MP denotes the vector
of market prices (which is a vector of 1s).

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
    model with the Standard Interpretation (SI);

2.  `ppnewint1`: a function to estimate a basic circulating capital
    model with the New Interpretation (NI);

3.  `ppsraffa1`: a function to estimate a circulating capital model with
    the Sraffian approach;

4.  `nonregdist`: a function that computes various non-regression-based
    measures of deviation between the vector of prices of production,
    the vector of market prices and the vector of direct prices (labor
    values);

5.  `createdata`: a function to create the data objects (matrices,
    vectors and scalars) necessary to implement the SI and NI.

The package contains the following three datasets.

1.  `aussea`: the socio economic accounts (SEA) for the Australian
    economy extracted from the 2016 release of the World Input Output
    Database (WIOD); this data set contains industry-level variables (53
    industries) for the USA for 15 years, 2000-2014;

2.  `ausiot`: input-output (IO) tables for the Australian economy
    extracted from the 2016 release of the World Input Output Database;
    this data set contains 53-industry input-output tables for the USA
    for 15 years, 2000-2014;

3.  `usasea`: the socio economic accounts (SEA) for the USA extracted
    from the 2016 release of the World Input Output Database; this data
    set contains industry-level variables (53 industries) for the USA
    for 15 years, 2000-2014;

4.  `usaiot`: input-output tables for the USA extracted from the 2016
    release of the World Input Output Database; this data set contains
    53-industry input-output tables for the USA for 15 years, 2000-2014;

5.  `usarwb`: personal consumption expenditure on the output of the 53
    industries of the input-output tables for the USA extracted from the
    2016 release of the World Input Output Database; this data set
    contains data for 15 years, 2000-2014. (Note: This data set is not
    necessary for the analysis.)

To see the correct syntax, required inputs and the output of the
functions, use the `help` function in R.

## Example 1: Analysis for USA, 2010

Let us conduct price of production analysis for Australia (AUS) and see
how to use the functions in `clptheory` to

1.  compute the uniform rate of profit and the vectors of labor values
    and prices of production for a basic circulating capital model using
    the Standard Interpretation and the New Interpretation; and

2.  compute various non-regression-based measures of deviation between
    PP/MP, DP/MP and PPDP.

Let us load the package.

``` r
# Load library
library(clptheory)
```

### Data

Let us create the data objects from the WIOD data base using the
`createdata` function. To use this function, the user needs to supply
the name of the country code (in this case “USA”), the year (in this
case 2010), the name of the SEA data set (in this case `usasea`) and the
name of the IO data set (in this case `usaiot`).

``` r
usadata <- clptheory::createdata(
  country = "USA", year = 2010, 
  datasea = usasea, dataio = usaiot
  )
#> "U"
```

### Standard Interpretation

Let us now estimate the circulating capital model with SI with the
`ppstdint1` function. To use this function, the user needs to supply the
A (input-output) matrix, the b (real wage bundle) vector, the Q (gross
output) vector, and the labor input (simple labor) vector.

``` r
si1 <- clptheory::ppstdint1(
  A = usadata$Ahat,
  b = usadata$b,
  Q = usadata$Q,
  l_simple = usadata$l_simple
)
```

### New Interpretation

Let us now estimate the circulating capital model with NI with the
`ppnewint1` function. To use this function, the user needs to supply the
A (input-output) matrix, the average nominal wage rate (scalar), the v
(value of labor power) scalar, the Q (gross output) vector, and the
labor input (simple labor) vector.

``` r
ni1 <- clptheory::ppnewint1(
  A = usadata$Ahat,
  w = usadata$wavg,
  v = usadata$vlp,
  Q = usadata$Q,
  l_simple = usadata$l_simple
)
```

Let us compare the uniform profit rates from SI and NI.

``` r
cbind(si1$urop,ni1$urop)
#>            [,1]      [,2]
#> [1,] -0.2816745 0.3709973
```

Here we see that the uniform rate of profit estimated by the SI and NI,
respectively, are -28.17 percent and 37.10 percent.

### Non-Regression-Based Measures of Deviation

Let us compute various non-regression-based measures of the deviation
between PP/MP, DP/MP and PP/DP for the SI using the `nonregdist`
function. To use this function, the user must supply the price of
production vector (in this case `si1$pp`), the direct prices vector (in
this case `si1$dp`), the vector of nominal wage rates (in this case
`usadata$wagevector_all`), the average wage rate as a scalar (in this
case `usadata$wavg`) and the gross output vector (in this case
`usadata$Q`).

``` r
nrsi1 <- clptheory::nonregdist(
  x = si1$pp,
  y = si1$dp,
  w = usadata$wagevector_all,
  w_avg = usadata$wavg,
  Q = usadata$Q
)
```

Let us do the same computation for the NI.

``` r
nrni1 <- clptheory::nonregdist(
  x=ni1$pp,
  y=ni1$dp,
  w=usadata$wagevector_all,
  w_avg=usadata$wavg,
  Q=usadata$Q
  )
```

We can now compare the results for the analysis of the *circulating
capital model* from the SI approach and the NI approach for the
non-regression-based measures of deviation between PP/MP, DP/MP and
PP/DP.

``` r
comp1 <- cbind(nrsi1,nrni1)

colnames(comp1) <- c("SI","NI")

rownames(comp1) <- c(
  "RMSE_PPMP", "RMSE_DPMP","RMSE_PPDP",
  "MAD_PPMP", "MAD_DPMP","MAD_PPDP",
  "MAWD_PPMP", "MAWD_DPMP","MAWD_PPDP",
  "Angle_PPMP", "Angle_DPMP","Angle_PPDP",
  "DDist_PPMP", "DDist_DPMP","DDist_PPDP"
)

# ---- The results
(comp1)
#>            SI         NI       
#> RMSE_PPMP  0.327702   1.774267 
#> RMSE_DPMP  0.2818772  0.2818772
#> RMSE_PPDP  0.07994628 1.649635 
#> MAD_PPMP   0.2691056  1.677533 
#> MAD_DPMP   0.229908   0.229908 
#> MAD_PPDP   0.06141611 1.623438 
#> MAWD_PPMP  0.3391636  1.575482 
#> MAWD_DPMP  0.2925821  0.2925821
#> MAWD_PPDP  0.07164429 1.639208 
#> Angle_PPMP 17.65202   12.59748 
#> Angle_DPMP 15.15651   15.15651 
#> Angle_PPDP 4.553346   6.429343 
#> DDist_PPMP 0.3068689  0.2194249
#> DDist_DPMP 0.2637604  0.2637604
#> DDist_PPDP 0.07944997 0.1121543
```

In the results above, we see the magnitudes of six different measures of
the deviation between PP/MP, DP/MP, PP/DP: root mean squared error
(RMSE), mean absolute distance (MAD), mean absolute weighted distance
(MAWD), angle between the two vectors (angle in degrees), and the
d-distance computed using angle (distance).

As an example, we can see that the d-distance of the deviation between
PP/MP, DP/MP, PP/DP for SI are 0.312, 0.275 and 0.043, respectively.
This can be interpreted as showing that the deviation between PP/MP,
DP/MP, PP/DP are 31.2 percent, 27.5 percent and 4.3 percent,
respectively. The corresponding measures of deviation for NI are 0.206,
0.275 and 0.088, respectively.

It is interesting to note that the measure of deviation for DP/MP are
the same in both SI and NI. The reason for this is as follows: the DP
vector is just a rescaled value of the labor value vector; hence it is
the same in both SI and NI. The MP vector is just a vector of 1s. Hence,
this vector is also same for both SI and NI.

### Doing an analysis for many years

*USA:* Let us compute the uniform rate of profit using the SI and NI for
the USA for several years. To do so, we will write some code to loop
over all the years in the data set: 2000-2014, i.e. 15 years.

``` r
# matrix to store results
urop <- matrix(NA, nrow = 15, ncol = 3)

# for loop
for(i in 1:15){
  # read data
  usadata <- clptheory::createdata(
  country = "USA", year = (1999+i), 
  datasea = usasea, dataio = usaiot
  )
  # estimate SI
  si1 <- clptheory::ppstdint1(
  A = usadata$Ahat,
  b = usadata$b,
  Q = usadata$Q,
  l_simple = usadata$l_simple
  )
  # estimate NI
  ni1 <- clptheory::ppnewint1(
  A = usadata$Ahat,
  w = usadata$wavg,
  v = usadata$vlp,
  Q = usadata$Q,
  l_simple = usadata$l_simple
  )
  # store result on the uniform rate of profit
  urop[i,] <- c(1999+i,si1$urop,ni1$urop)
}
```

Now let us see the results

``` r
# convert matrix of results to a data frame
urop_result <- as.data.frame(urop)
# supply column names
colnames(urop_result) <- c("year","urop(SI)","urop(NI)")
# print results
(urop_result)
#>    year   urop(SI)  urop(NI)
#> 1  2000 -0.2066772 0.2858142
#> 2  2001 -0.2124703 0.2891242
#> 3  2002 -0.2276504 0.3135421
#> 4  2003 -0.2348413 0.3223199
#> 5  2004 -0.2386104 0.3296036
#> 6  2005 -0.2375231 0.3336100
#> 7  2006 -0.2379900 0.3414167
#> 8  2007 -0.2416602 0.3248530
#> 9  2008 -0.2492589 0.3087132
#> 10 2009 -0.2784560 0.3494991
#> 11 2010 -0.2816745 0.3709973
#> 12 2011 -0.2864402 0.3584673
#> 13 2012 -0.2736577 0.3556414
#> 14 2013 -0.2815666 0.3595651
#> 15 2014 -0.2767634 0.3530419
# standard deviation of urop: SI
stats::sd(urop_result[,c("urop(SI)")])
#> [1] 0.02660856
# standard deviation of urop: NI
stats::sd(urop_result[,c("urop(NI)")])
#> [1] 0.02586745
```

From the last two lines above, we see that the uniform rate of profit
estimated by the SI has similar variability as the uniform rate of
profit estimated by the NI. Moreover, the uniform rate of profit
estimated by the SI is negative for all years!

*Australia:* Let us compute the uniform rate of profit using the SI and
NI for Australia for several years. Just like in the case of the USA, we
will write some code to loop over all the years in the data set:
2000-2014, i.e. 15 years.

``` r
# matrix to store results
urop <- matrix(NA, nrow = 15, ncol = 3)

# for loop
for(i in 1:15){
  # read data
  ausdata <- clptheory::createdata(
  country = "AUS", year = (1999+i), 
  datasea = aussea, dataio = ausiot
  )
  # estimate SI
  si1 <- clptheory::ppstdint1(
  A = ausdata$Ahat,
  b = ausdata$b,
  Q = ausdata$Q,
  l_simple = ausdata$l_simple
  )
  # estimate NI
  ni1 <- clptheory::ppnewint1(
  A = ausdata$Ahat,
  w = ausdata$wavg,
  v = ausdata$vlp,
  Q = ausdata$Q,
  l_simple = ausdata$l_simple
  )
  # store result on the uniform rate of profit
  urop[i,] <- c(1999+i,si1$urop,ni1$urop)
}
```

Now let us see the results

``` r
# convert matrix of results to a data frame
urop_result <- as.data.frame(urop)
# supply column names
colnames(urop_result) <- c("year","urop(SI)","urop(NI)")
# print results
(urop_result)
#>    year    urop(SI)  urop(NI)
#> 1  2000  0.13938777 0.4383862
#> 2  2001  0.26887556 0.4850732
#> 3  2002  0.27466405 0.4756530
#> 4  2003  0.09880047 0.4691115
#> 5  2004 -0.10018376 0.4133871
#> 6  2005 -0.09494687 0.4012927
#> 7  2006 -0.04794979 0.4122654
#> 8  2007 -0.16101663 0.3746102
#> 9  2008 -0.12245767 0.3770067
#> 10 2009 -0.02188953 0.4022079
#> 11 2010 -0.17690047 0.3485714
#> 12 2011 -0.20268669 0.3101514
#> 13 2012 -0.27377312 0.3138961
#> 14 2013 -0.31690479 0.3328282
#> 15 2014 -0.26880265 0.3607879
# standard deviation of urop: SI
stats::sd(urop_result[,c("urop(SI)")])
#> [1] 0.1875115
# standard deviation of urop: NI
stats::sd(urop_result[,c("urop(NI)")])
#> [1] 0.05632964
```

From the last two lines, We now see that the uniform rate of profit
estimated by the SI has much higher variability than the uniform rate of
profit estimated by the NI.

## Example 2: Simple 3-Industry Set-up

This example was presented on pages 46-57 of E. M. Ochoa’s dissertation
(Ochoa, E. M. 1984. Labor-Value and Prices of Production: An
Interindustry Study of the U.S. Economy, 1947–1972. PhD thesis, *New
School for Social Research*, New York, NY.).

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
  ncol=1)


# Capital stock coefficient matrix
K <- matrix(
  data = c(0,0,0,0.120,0.791,0.096,0.037,0.251,0.043),
  nrow=3, ncol=3, byrow = TRUE
)

# Diagonal matrix of turnover times
t <- diag(c(0.317, 0.099, 0.187))

# Uniform nominal wage rate
wavg <- 3.765

# Vector of nominal wage rates
w <- matrix(
  data = rep(wavg,3),
  ncol = 1
)

# Value of labor power
v <- 2/3

# Matrix of tax rates (assumed 0)
Tax <- matrix(0,nrow=3,ncol=3)
```

### Standard Interpretation

We will first implement the classical theory of prices for the
circulating capital model and then turn to the capital stock model.

#### Circulating Capital Model

``` r

# Estimate circulating capital model with SI
si1 <- ppstdint1(
  A = A,
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

What is the vector of direct prices?

``` r
si1$dp
#>          [,1]     [,2]      [,3]
#> [1,] 0.238526 4.197091 0.4869603
```

What is the vector of prices of production?

``` r
si1$pp
#> [1] 0.2606593 4.4614969 0.4139522
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
nrsi1 <- nonregdist(
  x = si1$pp,
  y = si1$dp,
  w = w,
  w_avg = wavg,
  Q = Q
)
```

#### Capital Stock Model

``` r
# Estimate model
si2 <- ppstdint2(
  A = A,
  l = l,
  b = b,
  Q = Q,
  D = D,
  K = K,
  t = t,
  Tax = Tax
)
```

What is the uniform rate of profti?

``` r
si2$urop
#> [1] 0.2337492
```

This exactly matches what is reported in equation (37) on page 57 in
Ochoa (1984). What is the vector of labor values?

``` r
si2$lvalues
#>           [,1]     [,2]      [,3]
#> [1,] 0.5192079 8.309406 0.9407729
```

This is exactly equal to what is reported in equation (11) on page 47 in
Ochoa (1984). What is the vector of direct prices?

``` r
si2$dp
#>           [,1]     [,2]      [,3]
#> [1,] 0.2627846 4.205606 0.4761495
```

These are different from what is reported in equation (11) on page 48 in
Ochoa (1984). The reason for this difference is that he uses a vector of
market prices, `(4, 60, 7)` for normalization that is different from a
vector of 1s (which is the common assumption in real world analysis).
What is the vector of prices of production?

``` r
si2$pp
#> [1] 0.3816756 4.2560306 0.4210263
```

These are different, once again because of the different normalization,
from what is reported in equation (38) on page 57 in Ochoa (1984). Let
us now compute the non-regression-based measures of deviation.

``` r
nrsi2 <- nonregdist(
  x = si2$pp,
  y = si2$dp,
  w = w,
  w_avg = wavg,
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
  w = wavg,
  v = v,
  Q = Q,
  l_simple = l)
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

What is the vector of direct prices?

``` r
ni1$dp
#>          [,1]     [,2]      [,3]
#> [1,] 0.238526 4.197091 0.4869603
```

What is the vector of prices of production?

``` r
ni1$pp
#>          [,1]     [,2]     [,3]
#> [1,] 2.621099 45.46782 4.702932
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
nrni1 <- nonregdist(
  x=ni1$pp,
  y=ni1$dp,
  w=w,
  w_avg=wavg,
  Q=Q)
```

#### Capital Stock Model

``` r
ni2 <- ppnewint2(
  A = A,
  l = l,
  v = v,
  w = wavg,
  Q = Q,
  D = D,
  K = K,
  t = t,
  Tax = Tax
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

What is the vector of direct prices?

``` r
ni2$dp
#>           [,1]     [,2]      [,3]
#> [1,] 0.2627846 4.205606 0.4761495
```

What is the vector of prices of production?

``` r
ni2$pp
#>          [,1]    [,2]     [,3]
#> [1,] 3.904408 48.3205 5.016769
```

Let us now compute the various non-regression-based measures of
deviation between the vector of all possible relative labor values and
the vector of all possible relative prices of production.

``` r
nrni2 <- nonregdist(
  x=ni2$pp,
  y=ni2$dp,
  w=w,
  w_avg=wavg,
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

rownames(comp1) <- c(
  "RMSE_PPMP", "RMSE_DPMP","RMSE_PPDP",
  "MAD_PPMP", "MAD_DPMP","MAD_PPDP",
  "MAWD_PPMP", "MAWD_DPMP","MAWD_PPDP",
  "Angle_PPMP", "Angle_DPMP","Angle_PPDP",
  "DDist_PPMP", "DDist_DPMP","DDist_PPDP"
)

colnames(comp1) <- c("SI","NI")

# ---- The results
(comp1)
#>            SI        NI        
#> RMSE_PPMP  2.071395  25.77936  
#> RMSE_DPMP  1.920455  1.920455  
#> RMSE_PPDP  0.1081    9.51179   
#> MAD_PPMP   1.595628  16.59728  
#> MAD_DPMP   1.490535  1.490535  
#> MAD_PPDP   0.1019051 9.493214  
#> MAWD_PPMP  1.061069  9.484922  
#> MAWD_DPMP  0.9800192 0.9800192 
#> MAWD_PPDP  0.1238156 9.135781  
#> Angle_PPMP 54.29756  53.93064  
#> Angle_DPMP 53.49691  53.49691  
#> Angle_PPDP 7.526052  3.967176  
#> DDist_PPMP 0.9126053 0.9069022 
#> DDist_DPMP 0.9001488 0.9001488 
#> DDist_PPDP 0.13126   0.06922645
```

We can compare the results for the analysis of the *capital stock model*
from the SI approach and the NI approach for the non-regression-based
measures of deviation between relative prices of production and relative
values.

``` r

comp2 <- cbind(nrsi2, nrni2)

rownames(comp2) <- c(
  "RMSE_PPMP", "RMSE_DPMP","RMSE_PPDP",
  "MAD_PPMP", "MAD_DPMP","MAD_PPDP",
  "MAWD_PPMP", "MAWD_DPMP","MAWD_PPDP",
  "Angle_PPMP", "Angle_DPMP","Angle_PPDP",
  "DDist_PPMP", "DDist_DPMP","DDist_PPDP"
)

colnames(comp2) <- c("SI","NI")

# ---- The results
(comp2)
#>            SI        NI       
#> RMSE_PPMP  1.942444  27.46998 
#> RMSE_DPMP  1.923002  1.923002 
#> RMSE_PPDP  0.269714  11.44563 
#> MAD_PPMP   1.484443  18.08056 
#> MAD_DPMP   1.48889   1.48889  
#> MAD_PPDP   0.1933954 11.2945  
#> MAWD_PPMP  0.9980861 10.40486 
#> MAWD_DPMP  0.9826291 0.9826291
#> MAWD_PPDP  0.1752105 10.64949 
#> Angle_PPMP 52.85015  53.00857 
#> Angle_DPMP 53.37635  53.37635 
#> Angle_PPDP 14.95225  10.46335 
#> DDist_PPMP 0.8900543 0.8925295
#> DDist_DPMP 0.8982692 0.8982692
#> DDist_PPDP 0.2602261 0.1823662
```

## References

- Basu, D. and Moraitis, T. (2023). Alternative Approaches to Labor
  Values and Prices of Production: Theory and Evidence. *Economics
  Department Working Paper Series*. 347. University of Massachusetts
  Amherst. Retrieved from
  <https://scholarworks.umass.edu/entities/publication/03270380-ed74-4f50-8c39-cc6325bfd05f>
