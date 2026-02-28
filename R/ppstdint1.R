#' Circulating capital model 1 using the Standard Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a basic circulating capital model using the Standard Interpretation.
#'
#' @param A input-output matrix (n x n).
#' @param b vector real wage bundle (n x 1).
#' @param Q gross output vector (n x 1).
#' @param l_simple vector of simple labor input (1 x n).
#'
#'
#' @importFrom popdemo isIrreducible
#'
#' @return A list with the following elements:
#' \item{meig}{Maximum eigen value of M}
#' \item{urop}{Uniform rate of profit (as a fraction)}
#' \item{mrop}{Maximum rate of profit (as a fraction)}
#' \item{ppabs}{Price of production vector (absolute)}
#' \item{pprel}{Price of production vector (relative)}
#' \item{lvalues}{Labor values vector}
#' \item{dprice}{Direct price vector}
#' \item{mevg}{Monetary expression of value using gross output}
#' \item{mnonneg}{Is M Nonnegative? (1=Y,0=N)}
#' \item{mirred}{Is M Irreducible? (1=Y,0=N)}
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
#' # Compute prices of production
#' ppstdint1(A = A,b = b,Q = Q,l_simple = l)
#'
ppstdint1 <- function(A, b, Q, l_simple){
  
  # -- Inputs to the function
  # A (nxn): input output matrix
  # l_simple (1xn): direct labor vector (adjusted for complexity)
  # b (nx1): real wage vector
  # Q (nx1): gross output vector
  
  # ---- M
  M <- (A + b%*%l_simple)
  
  # Is M nonnegative?
  nn_M <- ifelse(min(M)>=0,1,0)
  # Is M irreducible?
  ir_M <- ifelse(popdemo::isIrreducible(M),1,0)
  
  # ---- Uniform rate of profit
  maxEigenv <- max(Mod(eigen(M)$values))
  r <- (1/maxEigenv)-1
  
  # -- Maximal rate of profit (when b is the 0 vector)
  R <- 1/(max(Mod(eigen(A)$values)))-1
  
  # ----- Solve for price of production vector
  # Rel Price = First column of eigenvector matrix of M
  # The vector has all real elements (of the same sign)
  # If any element <0 then all elements <0; Hence, multiply with -1
  # transpose used because left eigenvector is needed
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
  colnames(p_pp) <- colnames(l_simple)
  
  
  # ---- Vector of values 
  # Note: we use the labor input adjusted for complexity
  I <- diag(ncol(A))
  lambda <- l_simple%*%solve(I - A)
  colnames(lambda) <- colnames(l_simple)
  
  # ---- Vector of Direct prices
  # Normalization defines alpha
  myalpha <- (lambda%*%Q)/sum(Q)
  # direct prices
  p_direct <- lambda/myalpha[1,1]
  colnames(p_direct) <- colnames(lambda)
  
  # ----- Results as a list
  return(list("Max Eigen Value (M)" = maxEigenv,
              "Uniform Rate of Profit" = r,
              "Maximal Rate of Profit" = R,
              "Prices of Production" = p_pp,
              "Direct prices" = p_direct,
              "Values" = lambda,
              "M: Nonnegative (1=Y,0=N)" = nn_M,
              "M: Irreducible (1=Y,0=N)" = ir_M
  )
  )
}
