#' Regression-based Measures of Deviation.
#'
#' This function computes various regression based measures of deviation between the vector of all possible relative labor values and the vector of all possible relative prices of production. It runs a log-log and a level-level regression of relative prices on relative values and tests the joint null hypothesis that the intercept is 0 and the slope is 1. 
#'
#' @param x price vector (1 x n).
#' @param y value vector (1 x n).
#' 
#' @importFrom car linearHypothesis
#' @importFrom utils combn
#' @importFrom stats lm
#' @importFrom stats nobs
#' 
#' @return A list with the following elements:
#' \item{a0lg}{Intercept in the log-log regression}
#' \item{a1lg}{Slope in the log-log regression}
#' \item{r2lg}{R-squared in the log-log regression}
#' \item{fstatlg}{F-stat of the null hypothesis that a0=0 and a1=1 in the log-log regression}
#' \item{pvallg}{P-value of the null hypothesis that a0=0 and a1=1 in the log-log regression}
#' \item{nlg}{Number of observations in the log-log regression}
#' \item{a0lv}{Intercept in the level-level regression}
#' \item{a1lv}{Slope in the level-level regression}
#' \item{r2lv}{R-squared in the level-level regression}
#' \item{fstatlv}{F-stat of the null hypothesis that a0=0 and a1=1 in the level-level regression}
#' \item{pvallv}{P-value of the null hypothesis that a0=0 and a1=1 in the level-level regression}
#' \item{nlv}{Number of observations in the level-level regression}
#' 
#' 
#'
#'@references Basu, Deepankar and Moraitis, Athanasios, "Alternative Approaches to Labor Values andPrices of Production: Theory and Evidence" (2023). Economics Department Working Paper Series. 347. URL: https://scholarworks.umass.edu/econ_workingpaper/347/
#'
#' @export
#'
#' @examples
#'
#' 
#' # Input-output matrix
#' A <- matrix(
#' data = c(0.265,0.968,0.00681,0.0121,0.391,0.0169,0.0408,0.808,0.165),
#' nrow=3, ncol=3, byrow = TRUE
#' )
#' # Direct labor input vector (complex)
#' l <- matrix(
#' data = c(0.193, 3.562, 0.616),
#' nrow=1
#' )
#' # Real wage bundle
#' b <- matrix(
#' data = c(0.0109, 0.0275, 0.296),
#' ncol=1
#' )
#' # Gross output vector
#' Q <- matrix(
#' data = c(26530, 18168, 73840),
#' ncol=1
#' )
#' # Direct labor input vector (simple)
#' l_simple <- l
#' # Market price vector
#' m <- matrix(data = c(4, 60, 7),nrow=1)
#' # Uniform nominal wage rate
#' wavg <- m%*%b
#' # Vector of nominal wage rates
#' w <- matrix(data=rep(wavg,3),nrow=1)
#' # Value of labor power
#' v <- 2/3
#' # Compute prices of production using NI
#' ni1 <- ppnewint1(A = A,l = l,w = wavg[1,1],v=v,Q = Q,l_simple = l)
#' # Regression-based measures of deviation
#' regtestrel(x=ni1$ppabs,y=ni1$lvalues)

regtestrel <- function(x,y){
  
  # Remove any zero prices
  mydat <- data.frame(x=as.vector(x),y=as.vector(y))
  mydat1 <- mydat[mydat$x!=0, ]
  
  # All possible relative prices
  x2 <- utils::combn(mydat1$x, 2)
  relp <- x2[1,]/x2[2,]
  
  # All possible relative values
  y2 <- utils::combn(mydat1$y, 2)
  relv <- y2[1,]/y2[2,]
  
  # Run regression
  dev1 <- stats::lm(log(relp) ~ log(relv))
  dev2 <- stats::lm(relp ~ relv)
  
  # Conduct F-test
  l1 <- car::linearHypothesis(dev1, c("(Intercept)=0","log(relv)=1"))
  l2 <- car::linearHypothesis(dev2, c("(Intercept)=0","relv=1"))
  
  # Return result
  return(
    list(
      a0lg = dev1$coefficients[1],
      a1lg = dev1$coefficients[2],
      r2lg = summary(dev1)$r.squared,
      fstatlg = l1$F[2],
      pvallg = round(l1$`Pr(>F)`[2], digits=4),
      nlg = nobs(dev1),
      a0lv = dev2$coefficients[1],
      a1lv = dev2$coefficients[2],
      r2lv = summary(dev2)$r.squared,
      fstatlv = l2$F[2],
      pvallv = round(l2$`Pr(>F)`[2], digits=4),
      nlv = nobs(dev2)
    )
  )
}