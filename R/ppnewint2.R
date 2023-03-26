#' Circulating capital model 2 using the New Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a circulating capital model using the New Interpretation. The model allows differential wage rates across industries but does not take account of unproductive labor for labor value calculations.
#'
#' @param A input-output matrix (n x n).
#' @param l vector of complex labor input (1 x n).
#' @param w vector of nominal wage rates (1 x n).
#' @param v value of labor power (scalar)
#' @param Q gross output vector (n x 1).
#' @param l_simple vector of simple labor input (1 x n).
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
#' # Uniform wage rate
#' wavg <- m%*%b 
#' # Vector of nominal wage rates
#' w <- matrix(data=c(wavg-0.5,wavg,wavg+0.5),nrow=1)
#' # Value of labor power
#' v <- 2/3
#' # Compute prices of production
#' ppnewint2(A = A,l = l,w = w[1,],v=v,Q = Q,l_simple = l)
#'

ppnewint2 <- function(A, l, w, v, Q, l_simple){

  # Necessary conditon
  if(v>=(l_simple%*%Q)/(l%*%Q)){
    stop("Necessary condition violated; Uniform rate of profit cannot be computed")
  } else{
    
    # Identity matrix 
    I <- diag(ncol(A))
    
    # Net output
    y <- (I-A)%*%Q
    
    # -- Maximum eigenvalue of A
    maxEigenv <- max(Mod(eigen(A)$values))
    
    # Is A nonnegative?
    nn_A <- ifelse(min(A)>=0,1,0)
    # Is A irreducible?
    ir_A <- ifelse(popdemo::isIrreducible(A),1,0)
    
    # -- Maximum rate of profit
    R <- (1/maxEigenv)-1
    
    
    # ----- Solve for uniform rate of profit
    
    # -- Define Univariate Function of rate of profit
    myfunc <- function(r2){
      
      t1 <- (1+r2)*(l%*%diag(w))%*%solve(I-(1+r2)*A)%*%y
      t2 <- ((l_simple%*%Q)*(l%*%diag(w)%*%Q))/(v*(l%*%Q))
      return(
        t1 - t2
      )
    }
    
    # Find root to get uniform rate of profit
    # Note: upper bound should be kept less than
    # R because the function blows up at R
    r <- stats::uniroot(myfunc,c(0,(R-0.00001)))$root
    
    # ----- Solve for price of production vector
    p_abs <- (1+r)*(l%*%diag(w))%*%solve(I-(1+r)*A)
    colnames(p_abs) <- colnames(l)
    
    # Vector of values 
    # Note: we use the labor input adjusted for complexity
    lambda <- l_simple%*%solve(I - A)
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
                Anonneg = nn_A,
                Airred = ir_A
              )
          )
  }
  
  
}
