#' Circulating capital model 1 using the Standard Interpretation.
#'
#' This function computes different measures of distance between prices of production, market prices and direct prices.
#'
#' @param x price of production vector (1 x n).
#' @param y direct prices vector (1 x n).
#' @param w vector of nominal wage rates (1 x n).
#' @param w_avg average wage rate (scalar).
#' @param Q gross output vector (1 x n)
#'
#'
#' @importFrom popdemo isIrreducible
#' @importFrom stats sd
#'
#' @return A list with the following elements:
#' \item{rmseppmp}{RMSE between price of production and market prices}
#' \item{rmsedpmp}{RMSE between direct prices and market prices}
#' \item{rmseppdp}{RMSE between prices of production and direct prices}
#' \item{madppmp}{MAD between price of production and market prices}
#' \item{maddpmp}{MAD between direct prices and market prices}
#' \item{madppdp}{MAD between prices of production and direct prices}
#' \item{mawdppmp}{MAWD between price of production and market prices}
#' \item{mawddpmp}{MAWD between direct prices and market prices}
#' \item{mawdppdp}{MAWD between prices of production and direct prices}
#' \item{angleppmp}{Angle between price of production and market prices}
#' \item{angledpmp}{Angle between direct prices and market prices}
#' \item{angleppdp}{Angle between prices of production and direct prices}
#' \item{ddistppmp}{D-distance between price of production and market prices}
#' \item{ddistdpmp}{D-distance between direct prices and market prices}
#' \item{ddistppdp}{D-distance between prices of production and direct prices}
#'
#'@references Basu, Deepankar and Moraitis, Athanasios, "Alternative Approaches to Labor Values andPrices of Production: Theory and Evidence" (2023). Economics Department Working Paper Series. 347. URL: https://scholarworks.umass.edu/econ_workingpaper/347/
#'
#' @export
#'
#' @examples
#'
#' # ------ Data
#' 
#' # price of production vector
#' x<- matrix(
#' data = c(0.25, 0.50, 0.75),
#' nrow=1
#' )
#' # direct price vector
#' y <- matrix(
#' data = c(0.33, 0.275, 0.85),
#' ncol=1
#' )
#' # Gross output vector
#' Q <- matrix(
#' data = c(26530, 18168, 73840),
#' ncol=1
#' )
#' # nominal wage rate vector
#' w <- matrix(
#' data = c(0.5, 0.33, 0.75),
#' ncol=1
#' )
#' # average wage (scalar)
#' w_avg <- 0.66
#' # Compute prices of production
#' nonregdist(x = x, y = y, Q = Q, w = w, w_avg = w_avg)
#'
nonregdist <- function(x,y,w,w_avg,Q){
  
  # Remove observations corresponding to zero prices
  mydat <- data.frame(
    x=as.vector(x),
    y=as.vector(y),
    w=as.vector(w),
    Q=as.vector(Q)
  )
  mydat1 <- mydat[mydat$x!=0, ]
  
  x <- mydat1$x
  y <- mydat1$y
  w <- mydat1$w
  Q <- mydat1$Q
  
  # --- RMSE% 
  rmse_abs_ppmp <- sqrt(mean((x-1)^2))
  rmse_abs_dpmp <- sqrt(mean((y-1)^2))
  rmse_abs_ppdp <- sqrt(mean(((x/y)-1)^2))
  
  # --- Mean Absolute Distance (MAD)
  mad_abs_ppmp <- mean(abs(x-1))
  mad_abs_dpmp <- mean(abs(y-1))
  mad_abs_ppdp <- mean(abs((x/y)-1))
  
  # --- Mean Absolute Weighted Distance 
  # --- Vector of weights
  omega_j <- Q/sum(Q)
  # --- MAWD
  mawd_abs_ppmp <- abs(x-1)%*%omega_j
  mawd_abs_dpmp <- abs(y-1)%*%omega_j
  mawd_abs_ppdp <- abs((x/y)-1)%*%omega_j
  
  # --- Angle in degrees with the unit vector
  tan_alpha_ppmp <- (sd(x)/mean(x))
  alpha_abs_ppmp <- (atan(tan_alpha_ppmp))*(180/pi)
  
  tan_alpha_dpmp <- (sd(y)/mean(y))
  alpha_abs_dpmp <- (atan(tan_alpha_dpmp))*(180/pi)
  
  tan_alpha_ppdp <- (sd(x/y)/mean(x/y))
  alpha_abs_ppdp <- (atan(tan_alpha_ppdp))*(180/pi)
  
  # -- D-distance using angle
  d_abs_ppmp <- sqrt(2*(1-cos(alpha_abs_ppmp*(pi/180))))
  d_abs_dpmp <- sqrt(2*(1-cos(alpha_abs_dpmp*(pi/180))))
  d_abs_ppdp <- sqrt(2*(1-cos(alpha_abs_ppdp*(pi/180))))
  
  
  
  # ---- Results ------- #
  # Return result
  return(
    list(
      rmseppmp = rmse_abs_ppmp, 
      rmsedpmp = rmse_abs_dpmp, 
      rmseppdp = rmse_abs_ppdp, 
      madppmp = mad_abs_ppmp,
      maddpmp = mad_abs_dpmp,
      madppdp = mad_abs_ppdp,
      mawdppmp = mawd_abs_ppmp, 
      mawddpmp = mawd_abs_dpmp, 
      mawdppdp = mawd_abs_ppdp, 
      angleppmp = alpha_abs_ppmp,
      angledpmp = alpha_abs_dpmp,
      angleppdp = alpha_abs_ppdp,
      ddistppmp = d_abs_ppmp,
      ddistdpmp = d_abs_dpmp,
      ddistppdp = d_abs_ppdp
    )
  )
  
}
