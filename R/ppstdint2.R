#' Capital stock model 1 using the Standard Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a capital stock model using the Standard Interpretation.
#'
#' @param A input-output matrix (n x n).
#' @param l vector of simple labor input (1 x n).
#' @param b vector real wage bundle (n x 1).
#' @param Q gross output vector (n x 1).
#' @param D depreciation matrix (n x n).
#' @param K capital stock coefficient matrix (n x n).
#' @param t diagonal matrix of turnover rates (n x n).
#' @param Tax matrix of tax rates (n x n).
#'
#'
#' @importFrom popdemo isIrreducible
#'
#' @return A list with the following elements:
#' \item{meig}{Maximum eigen value of M}
#' \item{urop}{Uniform rate of profit (as a fraction)}
#' \item{mrop}{Maximum rate of profit (as a fraction)}
#' \item{pp}{Price of production vector}
#' \item{dp}{Direct prices}
#' \item{lvalues}{Labor values vector}
#' \item{Mnonneg}{Is M Nonnegative? (1=Y,0=N)}
#' \item{Mirred}{Is M Irreducible? (1=Y,0=N)}
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
#' # Depreciation matrix
#' D <- matrix(
#' data = c(0,0,0,0.00568,0.0267,0.0028,0.00265,0.0147,0.00246),
#' nrow = 3, ncol = 3, byrow = TRUE
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
#' # Gross output vector 
#' Q <- matrix(
#' data = c(26530, 18168, 73840),
#' ncol = 1
#' )
#' # Capital stock coefficient matrix
#' K <- matrix(
#' data = c(0,0,0,0.120,0.791,0.096,0.037,0.251,0.043),
#' nrow=3, ncol=3, byrow=TRUE
#' )
#' # Diagonal matrix of turnover rates
#' t <- diag(c(0.317, 0.099, 0.187))
#' # Matrix of tax rates (assumed 0)
#' Tax <- matrix(0,nrow=3,ncol=3)
#' 
#' # Compute prices of production
#' ppstdint2(A=A,l=l,b=b,Q=Q,D=D,K=K,t=t,Tax=Tax)
#
ppstdint2 <- function(A, l, b, Q, D, K, t, Tax){
  
  I <- diag(ncol(A))
  # ---- M
  M <- (K + (A+b%*%l)%*%t)%*%solve(I-A-D-b%*%l-Tax)
  # Is M nonnegative?
  nn_M <- ifelse(min(M)>=0,1,0)
  # Is M irreducible?
  ir_M <- ifelse(popdemo::isIrreducible(M),1,0)
  
  # ---- Uniform rate of profit
  maxEigenv <- max(Mod(eigen(M)$values))
  r <- (1/maxEigenv)
  
  # -- Maximal rate of profit (when b is the 0 vector)
  R <- 1/(max(Mod(eigen(A)$values)))-1
  
  # ----- Solve for price of production vector
  # Rel Price = First column of eigen vector matrix of M
  # The vector has all real elements (of the same sign)
  # If any element <0 then all elements <0; Hence, multiply with -1
  p_rel_neg <- (-1)*Re(eigen(t(M))$vectors[,1])
  p_rel_pos <- Re(eigen(t(M))$vectors[,1])
  if (Re(eigen(t(M))$vectors[1,1])<0) {
    p_rel <- p_rel_neg
  }else{
    p_rel <- p_rel_pos
  }
  
  # Normalization defines beta
  mybeta <- (p_rel%*%Q)/sum(Q)
  # Price of production vector
  p_pp <- (1/mybeta[1,1])*p_rel
  #colnames(p_pp) <- colnames(l)
  
  
  # ---- Vector of values 
  # Note: we use the labor input adjusted for complexity
  lambda <- l%*%solve(I - A - D)
  #colnames(lambda) <- colnames(l)
  
  # ---- Vector of Direct prices
  # Normalization defines alpha
  myalpha <- (lambda%*%Q)/sum(Q)
  # direct prices
  p_direct <- lambda/myalpha[1,1]
  #colnames(p_direct) <- colnames(lambda)
  
  # ----- Results as a list
  return(list(meig = maxEigenv,
              urop = r,
              mrop = R,
              pp = p_pp,
              dp = p_direct,
              lvalues = lambda,
              Mnonneg = nn_M,
              Mirred = ir_M
  )
  )
}
