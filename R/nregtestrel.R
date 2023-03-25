#' Nonregression-based Measures of Deviation.
#'
#' This function computes various non-regression based measures of deviation between the vector of all possible relative labor values and the vector of all possible relative prices of production.
#'
#' @param x price vector (1 x n).
#' @param y value vector (1 x n).
#' @param w nominal wage rate vector (1 x n).
#' @param w_avg average nominal wage rate (scalar)
#' @param Q gross output vector (n x 1).
#' @param mev monetary expression of value using gross output (scalar)
#'
#' @importFrom stats sd
#' @importFrom utils combn
#' 
#' @return A list with the following elements:
#' \item{rmse}{Root mean squared error}
#' \item{mad}{Mean absolute distance}
#' \item{mawd}{Mean absolute weighted distance}
#' \item{cdm}{Classical distance measure}
#' \item{angle}{Angle between the two vectors (in degrees)}
#' \item{distangle}{Distance computed using the angle}
#' \item{lrelpplv}{Length of the relative price of production (or labor value) vector}
#' 
#'
#'@references Basu, Deepankar and Moraitis, Athanasios, "Alternative Approaches to Labor Values andPrices of Production: Theory and Evidence" (2023). Economics Department Working Paper Series. 347. URL: https://scholarworks.umass.edu/econ_workingpaper/347/
#'
#' @export
#'
#' @examples
#'
#' 
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
#' w <- matrix(data=rep(wavg,3),nrow=1)
#' # Value of labor power
#' v <- 2/3
#' # Compute prices of production using NI
#' ni1 <- ppnewint1(A = A,l = l,w = wavg[1,1],v=v,Q = Q,l_simple = l)
#' # Nonregression-based measures of deviation
#' nregtestrel(x=ni1$ppabs,y=ni1$lvalues,w=w,w_avg=wavg[1,1],mev=ni1$mevg,Q=Q)

nregtestrel <- function(x,y,w,w_avg,mev,Q){
  
  # Remove observations corresponding to zero prices
  mydat <- data.frame(
    x=as.vector(x),
    y=as.vector(y),
    w=as.vector(w),
    Q=as.vector(Q)
  )
  mydat1 <- mydat[mydat$x!=0, ]
  
  # ---------- Relative vectors (All Combinations) -------- #
  
  # All possible relative prices
  x2 <- utils::combn(mydat1$x, 2)
  relp_all <- x2[1,]/x2[2,]
  
  # All possible relative values
  y2 <- utils::combn(mydat1$y, 2)
  relv_all <- y2[1,]/y2[2,]
  
  # Length of relative price/value vectors
  lrelp <- length(relp_all)
  
  # ------------- Measures ---------------------- #
  # --- RMSE%
  rmse_rel_all <- sqrt(mean(((relp_all/relv_all)-1)^2))
  
  # --- Minimum Absolute Distance
  mad_rel_all <- mean(abs((relp_all/relv_all)-1))
  
  # --- Classical distance measure (CDM)
  # Relative wage vector
  w_rel <- mydat1$w/w_avg
  w_comb <- utils::combn(w_rel, 2)
  rel_w <- w_comb[1,]/w_comb[2,]
  
  # d vector
  d_j <- w_rel * mydat1$y
  d_comb <- utils::combn(d_j, 2)
  rel_d <- d_comb[1,]/d_comb[2,]
  
  # Vector of weights
  omega_j <- mydat1$Q/sum(mydat1$Q)
  omega_2 <- utils::combn(omega_j, 2)
  rel_omega <- omega_2[1,]*omega_2[2,]
  
  # CDM
  cdm_rel_all <- sum( abs((relp_all/rel_d)-1) * rel_omega )
  
  # --- Mean Absolute Weighted Distance
  mawd_rel_all <- sum(abs((relp_all/relv_all)-1)*rel_omega)
  
  # --- Angle in degrees
  z <- relp_all/relv_all
  tan_alpha <- (stats::sd(z)/mean(z))
  alpha_rel_all <- (atan(tan_alpha))*(180/pi)
  
  # -- Distance using angle
  d_rel_all <- sqrt(2*(1-cos(alpha_rel_all*(pi/180))))
  
  
  # ---- Results ------- #
  # Return result
  return(
    list(
      rmse = rmse_rel_all, 
      mad = mad_rel_all,
      mawd = mawd_rel_all, 
      cdm = cdm_rel_all,
      angle = alpha_rel_all,
      distangle = d_rel_all,
      lrelpplv = lrelp
    )
  )
  
}
