#' Circulating capital model 2 using the Standard Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a circulating capital model using the Standard Interpretation. The model has uniform wage rates across industries and takes into account unproductive labor for labor value calculations.
#'
#' @param A input-output matrix (n x n).
#' @param Ap input-output matrix for the subset of productive industries (m x m).
#' @param l vector of complex labor input (1 x n).
#' @param lp_simple vector of simple labor input for the subset of productive industries (1 x m).
#' @param b vector real wage bundle (n x 1).
#' @param Q gross output vector (n x 1).
#' @param Qp gross output vector for the subset of productive industries (m x 1).
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
#' ppstdint2(A=A,Ap=A[1:2,1:2],l=l,b=b,Q=Q,Qp=Q[1:2,1],lp_simple=l[1,1:2])
#'

ppstdint2 <- function(A, Ap, l, b, Q, Qp, lp_simple){
  
  # ---- M
  M <- A + b%*%l
  
  # Is M nonnegative?
  nn_M <- ifelse(min(M)>=0,1,0)
  # Is M irreducible?
  ir_M <- ifelse(popdemo::isIrreducible(M),1,0)
  
  # Perron-Frobenius theorem applies only if nn_M==1 and ir_M==1
  if(nn_M==0){
    stop("M is not nonnegative. Perron-Frobenius Theorem will not apply.")
  } else if(ir_M==0){
    stop("M is not irreducible. Perron-Frobenius Theorem will not apply.")
  } else{
    
    # ---- Uniform rate of profit
    maxEigenv <- max(Mod(eigen(M)$values))
    r <- (1/maxEigenv)-1
    
    # -- Maximal rate of profit (when b is the 0 vector)
    R <- 1/(max(Mod(eigen(A)$values)))-1
    
    # ----- Solve for price of production vector
    # Rel Price = First column of eigen vector matrix of M
    # The vector has all real elements (of the same sign)
    # If any element < 0, then all elements <0; hence, multiply with -1
    p_rel_neg <- (-1)*Re(eigen(M)$vectors[,1])
    p_rel_pos <- Re(eigen(M)$vectors[,1])
    if (Re(eigen(M)$vectors[1,1])<0) {
      p_rel <- p_rel_neg
    }else{
      p_rel <- p_rel_pos
    }
    
    # ---- Vector of values (for productive sectors only)
    # Note: we use the labor input adjusted for complexity
    Ip <- diag(ncol(Ap))
    lambda_p <- lp_simple%*%solve(Ip - Ap)
    colnames(lambda_p) <- colnames(lp_simple)
    
    # Normalization 1 using gross output
    mev_num_1 <- sum(matrix(Q,ncol=1))
    mev_den_1 <- (matrix(lambda_p,nrow=1)%*%matrix(Qp,ncol=1))
    mev_1 <- mev_num_1/mev_den_1
    
    # Normalization 2 using gross output
    mev_num_2 <- sum(matrix(Q,ncol=1))
    mev_den_2 <- (matrix(p_rel,nrow=1)%*%matrix(Q,ncol=1))
    mev_2 <- mev_num_2/mev_den_2
    
    # ----- Absolute price of production vector
    p_abs <- mev_2[1,1]*matrix(p_rel,nrow=1)
    colnames(p_abs) <- colnames(l)
    
    # Direct prices
    direct_p <- mev_1[1,1]*matrix(lambda_p, nrow = 1)
    colnames(direct_p) <- colnames(lp_simple)
    
    
    # ----- Results as a list
    return(list(meig = maxEigenv,
                urop = r,
                mrop = R,
                ppabs = p_abs,
                pprel = p_rel,
                lvalues = lambda_p,
                dprice = direct_p,
                mevg = mev_1[1,1],
                mnonneg = nn_M,
                mirred = ir_M
              )
          )
    
    }
  
}
