#' Capital stock model 1 using the New Interpretation.
#'
#' This function computes the uniform rate of profit, prices of production and labor values for a capital stock model using the New Interpretation.
#'
#' @param A input-output matrix (n x n).
#' @param l vector of simple labor input (1 x n).
#' @param w average nominal wage rate (scalar)
#' @param v value of labor power (scalar)
#' @param Q gross output vector (n x 1).
#' @param D depreciation matrix (n x n).
#' @param K capital stock coefficient matrix (n x n).
#' @param t diagonal matrix of turnover rates (n x n).
#' @param Tax matrix of tax rates (n x n).
#'
#' @importFrom popdemo isIrreducible
#' @importFrom stats uniroot
#'
#' @return A list with the following elements:
#' \item{meig}{Maximum eigen value of A}
#' \item{urop}{Uniform rate of profit (as a fraction)}
#' \item{mrop}{Maximum rate of profit (as a fraction)}
#' \item{pp}{Price of production vector}
#' \item{dp}{Direct prices}
#' \item{lvalues}{Labor values vector}
#' \item{Mnonneg}{Is M Nonnegative? (1=Y,0=N)}
#' \item{Mirred}{Is M Irreducible? (1=Y,0=N)}
#' \item{Nnonneg}{Is N Nonnegative? (1=Y,0=N)}
#' \item{Nirred}{Is N Irreducible? (1=Y,0=N)}
#' \item{MNirred}{Is M and N both Irreducible? (1=Y,0=N)}
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
#' # Value of labor power
#' v <- 2/3
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
#' # Matrix of tax rates (assumed 0 for this example)
#' Tax <- matrix(0,nrow=3,ncol=3)
#' # Average nominal wage rate
#' w <- 3.765
#' # Compute prices of production
#' ppnewint2(A=A,l=l,w=w,v=v,Q=Q,D=D,K=K,t=t,Tax=Tax)
#'
ppnewint2 <- function(A, l, w, v, Q, D, K, t, Tax){

# Identity matrix
I <- diag(ncol(A))

# ---- M
M <- (K + A%*%t)%*%solve(I-A-D-Tax)

# Is M nonnegative?
nn_M <- ifelse(min(M)>=0,1,0)
ir_M <- ifelse(popdemo::isIrreducible(M),1,0)


# ---- Maximum rate of profit
maxEigenv_M <- max(Mod(eigen(M)$values))
R <- (1/(maxEigenv_M))

# ---- Define N
N <- A+D+Tax + R*(K + A%*%t)
# Is N nonnegative?
nn_N <- ifelse(min(N)>=0,1,0)
# Is N irreducible?
ir_N <- ifelse(popdemo::isIrreducible(N),1,0)
# --- Are both M and N irreducible?
ir_MN <- ifelse((ir_M==1 & ir_N==1),1,0)


# ----- Solve for uniform rate of profit

# Net output
Y <- (I-A-D)%*%Q
# -- Define Univariate Function of rate of profit
myfunc <- function(r2){
  
  U <- A + D + Tax + r2*(A%*%t) + r2*K
  
  return(
    (w*l + r2*w*l%*%t)%*%solve(I-U)%*%Y - ((w*l)%*%Q)/v
  )
}
# Find root to get uniform rate of profit
# Note: upper bound should be kept less than
# R because the function blows up at R
r <- uniroot(myfunc,c(0,(R-0.00001)))$root

# ----- Solve for price of production vector
Ur <- A + D + Tax + r*(A%*%t) + r*K
p_pp <- (w*l + r*w*l%*%t)%*%solve(I-Ur)
colnames(p_pp) <- colnames(l)

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
return(list(meig = maxEigenv_M,
            mrop = R,
            urop = r,
            pp = p_pp,
            dp = p_direct,
            lvalues = lambda,
            Mnonneg = nn_M,
            Mirred = ir_M,
            Nnonneg = nn_N,
            Nirred = ir_N,
            MNirred = ir_MN
)
)
}
