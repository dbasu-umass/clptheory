#' Circulating capital model 1 using the New Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a basic circulating capital model using the Sraffian method. 
#'
#' @param A input-output matrix (n x n).
#' @param pshare profit share (scalar)
#' @param Q gross output vector (n x 1).
#' @param l_simple vector of simple labor input (1 x n).
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
#' # Gross output vector
#' Q <- matrix(
#' data = c(26530, 18168, 73840),
#' ncol=1
#' )
#' # Direct labor input vector (simple)
#' l_simple <- l
#' # Profit share
#' v <- 1/3
#' # Compute prices of production
#' ppsraffa1(A = A,pshare=pshare,Q = Q,l_simple = l)
#'
ppsraffa1 <- function(A,Q,pshare,l_simple){
  
  # -- Inputs to the function
  # A (nxn): input output matrix
  # l_simple (1xn): direct labor vector (adjusted for complexity)
  # Q (nx1): gross output vector
  # pshare : profit share (scalar)
  
  # ---- H
  I <- diag(ncol(A))
  H <- A%*%solve(I-A)
  
  # Is H nonnegative?
  nn_H <- ifelse(min(H)>=0,1,0)
  # Is H irreducible?
  require(popdemo)
  ir_H <- ifelse(popdemo::isIrreducible(H),1,0)
  
  # ---- Maximal rate of profit
  maxEigenv <- max(Mod(eigen(H)$values))
  R <- (1/maxEigenv)
  
  # --- Uniform rate of profit
  r <- pshare*R
  
  # ---- Vector of values
  I <- diag(ncol(A))
  lambda <- l_simple%*%solve(I - A)
  colnames(lambda) <- colnames(l_simple)
  
  
  # ------ Vector of direct prices
  # Normalization defines alpha
  myalpha <- (lambda%*%Q)/sum(Q)
  # direct prices
  p_direct <- lambda/myalpha[1,1]
  colnames(p_direct) <- colnames(lambda)
  
  # ---- Price of production vector
  I <- diag(ncol(H))
  # Price of production vector in terms of standard commodity
  p_abs <- (1-pshare)*lambda%*%solve(I - r*H)
  # Normalization defines beta
  mybeta <- (p_abs%*%Q)/sum(Q)
  # Price of production vector
  p_pp <- (1/mybeta[1,1])*p_abs
  colnames(p_pp) <- colnames(lambda)
  
  
  # ----- Output of function
  return(list(meig = maxEigenv,
              mrop = R,
              urop = r,
              pp = p_pp,
              dp = p_direct,
              lvalues = lambda,
              Hnonneg = nn_H,
              Airred = ir_H
  )
  )
}
