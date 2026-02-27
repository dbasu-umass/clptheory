#' Circulating capital model 1 using the New Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a basic circulating capital model using the New Interpretation.
#'
#' @param A input-output matrix (n x n).
#' @param l vector of complex labor input (1 x n).
#' @param w uniform nominal wage rate (scalar).
#' @param v value of labor power (scalar)
#' @param Q gross output vector (n x 1).
#' @param l_simple vector of simple labor input (1 x n).
#'
#' @importFrom popdemo isIrreducible
#'
#' @return A list with the following elements:
#' \item{meig}{Maximum eigen value of A}
#' \item{urop}{Uniform rate of profit (as a fraction)}
#' \item{mrop}{Maximum rate of profit (as a fraction)}
#' \item{pp}{Price of production vector}
#' \item{dp}{Direct prices}
#' \item{lvalues}{Labor values vector}
#' \item{Anonneg}{Is A Nonnegative? (1=Y,0=N)}
#' \item{Airred}{Is A Irreducible? (1=Y,0=N)}
#' 
#'@references Basu, Deepankar and Moraitis, Athanasios, "Alternative Approaches to Labor Values andPrices of Production: Theory and Evidence" (2023). Economics Department Working Paper Series. 347. URL: https://scholarworks.umass.edu/econ_workingpaper/347/
#'
#' @export
#'
#' @examples
#'
#' # ------ Data
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
#' # Value of labor power
#' v <- 2/3
#' # Compute prices of production
#' ppnewint1(A = A,w = wavg[1,1],v=v,Q = Q,l_simple = l)
#'
ppnewint1 <- function(A, w, v, Q, l_simple){
  
  # -- Inputs to the function
  # A (nxn): input output matrix
  # l_simple (1xn): direct labor inputs (adjusted for complexity)
  # w: average wage rate (scalar)
  # v: value of labor power (scalar)
  # Q (nx1): gross output vector
  
  # Identity matrix 
  I <- diag(ncol(A))
  
  # Net output
  y <- (I-A)%*%Q
  
  # -- Maximum eigenvalue of A
  maxEigenv <- max(Mod(eigen(A)$values))
  
  # Is A nonnegative?
  nn_A <- ifelse(min(A)>=0,1,0)
  # Is A irreducible?
  require(popdemo)
  ir_A <- ifelse(popdemo::isIrreducible(A),1,0)
  
  # -- Maximum rate of profit
  R <- (1/maxEigenv)-1
  
  
  # ----- Solve for uniform rate of profit
  
  # -- Define Univariate Function of rate of profit
  myfunc <- function(r2){
    
    return(
      (1+r2)*w*l_simple%*%solve(I-(1+r2)*A)%*%y - ((w*l_simple)%*%Q)/v
    )
  }
  
  # Find root to get uniform rate of profit
  # Note: upper bound should be kept less than
  # R because the function blows up at R
  r <- uniroot(myfunc,c(0,(R-0.00001)))$root
  
  # ----- Solve for price of production vector
  p_pp <- (1+r)*(w*l_simple)%*%solve(I-(1+r)*A)
  colnames(p_pp) <- colnames(l_simple)
  
  # Vector of values 
  # Note: we use the labor input adjusted for complexity
  lambda <- l_simple%*%solve(I - A)
  colnames(lambda) <- colnames(l_simple)
  
  # ---- Vector of Direct prices
  # Normalization defines alpha
  myalpha <- (lambda%*%Q)/sum(Q)
  # direct prices
  p_direct <- lambda/myalpha[1,1]
  colnames(p_direct) <- colnames(lambda)
  
  # Results as a list
  return(list(meig = maxEigenv,
              mrop = R,
              urop = r,
              pp = p_pp,
              dp = p_direct,
              lvalues = lambda,
              Anonneg = nn_A,
              Airred = ir_A
  )
  )
  
}
