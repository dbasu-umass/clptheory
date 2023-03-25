#' Capital stock model 1 using the New Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a basic capital stock model using the New Interpretation. The model has uniform wage rates across industries and does not take account of unproductive labor for labor value calculations.
#'
#' @param A input-output matrix (n x n).
#' @param l vector of complex labor input (1 x n).
#' @param w uniform nominal wage rate (scalar).
#' @param v value of labor power (scalar)
#' @param Q gross output vector (n x 1).
#' @param l_simple vector of simple labor input (1 x n).
#' @param D depreciation matrix (n x n).
#' @param K capital stock coefficient matrix (n x n).
#' @param t turnover times matrix (n x n diagonal).
#'
#' @importFrom popdemo isIrreducible
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
#' # Value of labor power
#' v <- 2/3
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
#' ppnewint5(A = A,l = l,w = wavg[1,1],v=v,Q = Q,l_simple = l,D=D,K=K,t=t)
#'

ppnewint5 <- function(A, l, w, v, Q, D, K, t, l_simple){
  
  # Necessary condition
  if(v>=(l_simple%*%Q)/(l%*%Q)){
    stop("Uniform rate of profit cannot be computed")
  } else{
    
    # Identity matrix 
    I <- diag(ncol(A))
    
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
      C1 <- (w*l + r2*w*l%*%t)
      
      return(
        (C1%*%B1%*%y) - ((w*l_simple)%*%Q)/v
      )
    }
    
    # Find root to get uniform rate of profit
    # Note: upper bound should be kept less than
    # R because the function blows up at R
    r <- stats::uniroot(myfunc,c(0,(R-0.00001)))$root
    
    # ----- Solve for price of production vector
    p_abs <- (w*l + r*w*l%*%t)%*%solve(I - A - D -r*K - r*A%*%t)
    
    # Vector of values 
    # Note: we use the labor input adjusted for complexity
    lambda <- l_simple%*%solve(I - A -D)
    colnames(lambda) <- colnames(l_simple)
    
    # MEV
    mev <- (p_abs%*%y)/(l %*%Q)
    
    # Monetary expression of value (using gross output)
    mev_gross <- (p_abs%*%Q)/(lambda%*%Q)
    
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
  }
  
}
