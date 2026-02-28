
<!-- README.md is generated from README.Rmd. Please edit that file -->

# clptheory

<!-- badges: start -->

<!-- badges: end -->

The goal of `clptheory` (*classical price theory*) is to create a suite
of functions to implement the classical theory of prices. The functions
in this package computes the uniform rate of profit, the vector of price
of production and the vector of labor values for the circulating capital
model and the capital stock model. The functions also computes various
regression- and non-regression-based measures of deviation between the
vector of relative prices of production and the vector of relative labor
values.

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
    between the vector of relative prices of production and the vector
    of relative labor values.

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
  w = ausdata$wavg,
  v = ausdata$vlp,
  Q = ausdata$Q,
  l_simple = ausdata$l_simple
)
```

Let us see the uniform profit rate.

``` r
cbind(si1$urop,ni1$urop)
#>            [,1]      [,2]
#> [1,] -0.1769005 0.3485714
```

### Non-Regression-Based Measures of Deviation

Let us compute various non-regression-based measures of the deviation
between the vector of prices of production and the vector of market
prices, between the vector of direct prices and market prices, and
between the vector of prices of production and the direct prices for the
SI.

``` r
nrsi1 <- clptheory::nonregdist(
  x = si1$pp,
  y = si1$dp,
  w = ausdata$wagevector_all,
  w_avg = ausdata$wavg,
  Q = ausdata$Q
)
```

Let us do the same computation for the NI.

``` r
nrni1 <- clptheory::nonregdist(
  x=ni1$pp,
  y=ni1$dp,
  w=ausdata$wagevector_all,
  w_avg=ausdata$wavg,
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
rownames(comp1) <- c(
  "RMSE_PPMP", "RMSE_DPMP","RMSE_PPDP",
  "MAD_PPMP", "MAD_DPMP","MAD_PPDP",
  "MAWD_PPMP", "MAWD_DPMP","MAWD_PPDP",
  "Angle_PPMP", "Angle_DPMP","Angle_PPDP",
  "DDist_PPMP", "DDist_DPMP","DDist_PPDP"
)
(comp1)
#>            SI         NI        
#> RMSE_PPMP  0.3123236  1.49563   
#> RMSE_DPMP  0.2733907  0.2733907 
#> RMSE_PPDP  0.04356873 1.541333  
#> MAD_PPMP   0.2089791  1.409501  
#> MAD_DPMP   0.1846344  0.1846344 
#> MAD_PPDP   0.03339916 1.525458  
#> MAWD_PPMP  0.3051267  1.462637  
#> MAWD_DPMP  0.2634856  0.2634856 
#> MAWD_PPDP  0.04477922 1.537114  
#> Angle_PPMP 17.96719   11.84869  
#> Angle_DPMP 15.78948   15.78948  
#> Angle_PPDP 2.443587   5.045813  
#> DDist_PPMP 0.3123034  0.2064304 
#> DDist_DPMP 0.2747072  0.2747072 
#> DDist_PPDP 0.0426454  0.08803759
```

In the results above, we see the magnitudes of six different measures of
the deviation between PP/MP, DP/MP, PP/DP: root mean squared error
(RMSE), meann absolute distance (MAD), mean absolute weighted distance
(MAWD), classical distance measure (CDM), angle between the two vectors
(angle in degrees), and distance computed using angle (distance).

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

## References

- Basu, D. and Moraitis, T. (2023). Alternative Approaches to Labor
  Values and Prices of Production: Theory and Evidence. *Economics
  Department Working Paper Series*. 347. University of Massachusetts
  Amherst. Retrieved from
  <https://scholarworks.umass.edu/econ_workingpaper/347/>
