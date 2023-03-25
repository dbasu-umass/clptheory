#' Capital stock model 1 using the Standard Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a basic capital stock model using the Standard Interpretation. The model has uniform wage rates across industries and does not take into account unproductive labor for labor value calculations.
#'
#' @param A input-output matrix (n x n).
#' @param l vector of complex labor input (1 x n).
#' @param b vector real wage bundle (n x 1).
#' @param Q gross output vector (n x 1).
#' @param l_simple vector of simple labor input (1 x n).
#' @param D depreciation matrix (n x n).
#' @param K capital stock coefficient matrix (n X n).
#' @param t turnover matrix (n x n diagonal matrix).
#'
#' @importFrom popdemo isIrreducible
#'
#' @return A list with the following elements:
#' \item{meig}{Maximum eigen value of N}
#' \item{urop}{Uniform rate of profit (as a fraction)}
#' \item{mrop}{Maximum rate of profit (as a fraction)}
#' \item{ppabs}{Price of production vector (absolute)}
#' \item{pprel}{Price of production vector (relative)}
#' \item{lvalues}{Labor values vector}
#' \item{dprice}{Direct price vector}
#' \item{mevg}{Monetary expression of value using gross output}
#' \item{nnonneg}{Is N Nonnegative? (1=Y,0=N)}
#' \item{nirred}{Is N Irreducible? (1=Y,0=N)}
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
#' ppstdint3(A = A,l = l,b = b,Q = Q,l_simple = l,D=D,K=K,t=t)
#'

ppstdint3 <- function(A, l, b, Q, D, K, t, l_simple){
  
  # Identity matrix 
  I <- diag(ncol(A))
  
  # -- Define N
  N <- (K + (A+b%*%l)%*%t)%*%solve(I-A-D-b%*%l)
  
  # Is N nonnegative?
  nn_N <- ifelse(min(N)>=0,1,0)
  # Is N irreducible?
  ir_N <- ifelse(popdemo::isIrreducible(N),1,0)
  
  # Perron-Frobenius theorem applies only if nn_N==1 and ir_N==1
  if(nn_N==0){
    stop("N is not nonnegative. Perron-Frobenius Theorem will not apply.")
  } else if(ir_N==0){
    stop("N is not irreducible. Perron-Frobenius Theorem will not apply.")
  } else{
    
    # -- Uniform rate of profit
    maxEigenv <- max(Mod(eigen(N)$values))
    r <- (1/maxEigenv)
    
    # -- Maximal rate of profit (when b is the 0 vector)
    M <- (K + (A)%*%t)%*%solve(I-A-D)
    R <- 1/(max(Mod(eigen(M)$values)))
    
    # ----- Solve for price of production vector
    # Rel Price = First column of eigen vector matrix of N
    # The vector has all real elements (of the same sign)
    # If all elements <0, multiply with -1
    p_rel_neg <- (-1)*Re(eigen(N)$vectors[,1])
    p_rel_pos <- Re(eigen(N)$vectors[,1])
    if (Re(eigen(N)$vectors[1,1])<0) {
      p_rel <- p_rel_neg
    }else{
      p_rel <- p_rel_pos
    }
    
    # Vector of values 
    # Note: we use the labor input adjusted for complexity
    lambda <- l_simple%*%solve(I - A -D)
    colnames(lambda) <- colnames(l_simple)
    
    # Normalization 1 using gross output
    mev_num_1 <- sum(matrix(Q,ncol=1))
    mev_den_1 <- (matrix(lambda,nrow=1)%*%matrix(Q,ncol=1))
    mev_1 <- mev_num_1/mev_den_1
    
    # Normalization 2 using gross output
    mev_num_2 <- sum(matrix(Q,ncol=1))
    mev_den_2 <- (matrix(p_rel,nrow=1)%*%matrix(Q,ncol=1))
    mev_2 <- mev_num_2/mev_den_2
    
    # ----- Absolute price of production vector
    p_abs <- mev_2[1,1]*matrix(p_rel,nrow=1)
    colnames(p_abs) <- colnames(l)
    
    # Direct prices
    direct_p <- mev_1[1,1]*matrix(lambda, nrow = 1)
    colnames(direct_p) <- colnames(l_simple)
    
    
    # ----- Results as a list
    return(list(meig = maxEigenv,
                urop = r,
                mrop = R,
                ppabs = p_abs,
                pprel = p_rel,
                lvalues = lambda,
                dprice = direct_p,
                mevg = mev_1[1,1],
                nnonneg = nn_N,
                nirred = ir_N
              )
         )
  }
  
  
  
}
