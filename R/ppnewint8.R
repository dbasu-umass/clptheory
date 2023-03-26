#' Capital stock model 4 using the New Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a capital stock model using the New Interpretation. The model allows differential wage rates across industries and takes account of unproductive labor for labor value calculations.
#'
#' @param A input-output matrix (n x n).
#' @param Ap input-output matrix for the subset of productive industries (m x m).
#' @param l vector of complex labor input (1 x n).
#' @param lp vector of complex labor input for the subset of productive industries (1 x m).
#' @param lp_simple vector of simple labor input for the subset of productive industries (1 x m).
#' @param w vector of nominal wage rates (1 x n).
#' @param wp vector of nominal wage rates for the subset of productive industries (1 x m).
#' @param v value of labor power (scalar).
#' @param Q gross output vector (n x 1).
#' @param Qp gross output vector for the subset of productive industries (m x 1).
#' @param D depreciation matrix (n x n).
#' @param Dp depreciation matrix for the subset of productive industries (m x m).
#' @param K capital stock coefficient matrix (n x n).
#' @param t turnover times matrix (n x n diagonal).
#'
#' @importFrom popdemo isIrreducible
#' @importFrom stats uniroot
#'
#' @return A list with the following elements:
#' \item{meig}{Maximum eigen value of A}
#' \item{urop}{Uniform rate of profit (as a fraction)}
#' \item{mrop}{Maximum rate of profit (as a fraction)}
#' \item{ppabs}{Price of production vector (absolute)}
#' \item{pprel}{Price of production vector (relative)}
#' \item{lvalues}{Labor values vector}
#' \item{mevn}{Monetary expression of value using net output}
#' \item{mevg}{Monetary expression of value using gross output}
#' \item{Nnonneg}{Is N Nonnegative? (1=Y,0=N)}
#' \item{Nirred}{Is N Irreducible? (1=Y,0=N)}
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
#' # Vector of nominal wage rates
#' w <- matrix(data=c(wavg-0.5,wavg,wavg+0.5),nrow=1)
#' # Value of labor power
#' v <- 3/5
#' # Depreciation matrix
#' D <- matrix(data = c(0,0,0,0.00568,0.0267,0.0028,0.00265,0.0147,0.00246),
#' nrow=3, ncol=3, byrow = TRUE
#' )
#' # Capital stock coefficient matrix
#' K <- matrix(
#' data = c(0,0,0,0.120,0.791,0.096,0.037,0.251,0.043),
#' nrow=3, ncol=3, byrow = TRUE
#' )
#' # Diagonal turnover matrix
#' t <- diag(c(0.317, 0.099, 0.187))
#' # Compute prices of production
#' ppnewint8(A=A,Ap=A[1:2,1:2],l=l,lp=l[1,1:2],w=w[1,],wp=w[1,1:2],v=v,
#' Q=Q,Qp=Q[1:2,1],lp_simple=l[1,1:2],D=D,Dp=D[1:2,1:2],K=K,t=t)
#'

ppnewint8 <- function(A, Ap, l, lp, w, wp, v, Q, Qp, D, Dp, K, t, lp_simple){
  
  # Necessary condition for solutions
  if(v>=(lp_simple%*%Qp)/(l%*%Q)){
    stop("Uniform rate of profit cannot be computed")
  } else{
    # Identity matrix 
    I <- diag(ncol(A))
    Ip <- diag(ncol(Ap))
    
    # Net output
    y <- (I-A-D)%*%Q
    
    # -- Maximum eigenvalue of N
    N <- (K + A%*%t)%*%solve(I-A-D)
    maxEigenv <- max(Mod(eigen(N)$values))
    
    # Is N nonnegative?
    nn_N <- ifelse(min(N)>=0,1,0)
    # Is N irreducible?
    ir_N <- ifelse(popdemo::isIrreducible(N),1,0)
    
    # -- Maximum rate of profit
    R <- (1/maxEigenv)
    
    
    # ----- Solve for uniform rate of profit
    
    # -- Define Univariate function of rate of profit
    myfunc <- function(r2){
      
      B1 <- solve(I - A - D - r2*K - r2*A%*%t)
      C1 <- (l%*%diag(w) + r2*l%*%diag(w)%*%t)
      wbar <- (l%*%diag(w)%*%Q)/(l%*%Q)
      
      return(
        (C1%*%B1%*%y) - ((lp_simple%*%Qp)*wbar)/(v)
      )
    }
    
    # Find root to get uniform rate of profit
    # Note: upper bound should be kept less than
    # R because the function blows up at R
    r <- stats::uniroot(myfunc,c(0,R-0.00001))$root
    
    # ----- Solve for price of production vector
    p_abs <- (l%*%diag(w) + r*l%*%diag(w)%*%t)%*%solve(I - A - D -r*K - r*A%*%t)
    
    # Vector of values 
    # Note: we use the labor input adjusted for complexity
    lambda <- lp_simple%*%solve(Ip - Ap -Dp)
    colnames(lambda) <- colnames(lp_simple)
    
    # MEV
    mev <- (p_abs%*%y)/(lp_simple %*%Qp)
    
    # Monetary expression of value (using gross output)
    mev_gross <- (p_abs%*%Q)/(lambda%*%Qp)
    
    # Results as a list
    return(list(meig = maxEigenv,
                mrop = R,
                urop = r,
                ppabs = p_abs,
                pprel = p_abs/p_abs[1],
                lvalues = lambda,
                mevn = mev[1,1],
                mevg = mev_gross[1,1],
                Nnonneg = nn_N,
                Nirred = ir_N
              )
          )
    
  } # end of if else statement
  
  
}
