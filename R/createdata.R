#' Create data set for analysis.
#'
#' This function creates the data objects (matrices, vectors and scalars) necessary to implement the SI and NI.
#'
#' @param country country code as a character (e.g. "USA").
#' @param year year (eg. 2000).
#' @param data_sea the socio economic accounts (data frame).
#' @param data_io the input-output (data frame).
#' @param rwb personal consumption expenditure (data frame).
#'
#' @import dplyr 
#' @import magrittr
#'
#' @return A list with the following elements:
#' \item{Ahat}{The input-output matrix}
#' \item{l}{The direct labor input vector (complex labor)}
#' \item{l_simple}{The direct labor input vector (simple labor)}
#' \item{Q}{The gross output vector}
#' \item{wavg}{The average or uniform nominal wage rate}
#' \item{wagevector_all}{The vector of nominal wage rates}
#' \item{vlp}{Value of labor power}
#' \item{b}{The consumption or real wage bundle}
#' \item{pshare}{Average profit share}
#'
#'@references Basu, Deepankar and Moraitis, Athanasios, "Alternative Approaches to Labor Values andPrices of Production: Theory and Evidence" (2023). Economics Department Working Paper Series. 347. URL: https://scholarworks.umass.edu/econ_workingpaper/347/
#'
#' @export
#'
#' @examples
#'
#' createdata(country="USA",year=2014,data_sea=usasea,data_io=usaiot,rwb=usarwb)
#' 
#'

createdata <- function(country,year,data_sea,data_io,rwb){
  
  # ---- The inputs
  # Country
  ctry <- country
  # Year
  yr <- year
  # SEA data
  # SEA data set
  d1 <- data_sea
  # National IO data
  d2 <- data_io
  
  
  # ----------- Using SEA Data --------------------- #
  
  # Industry codes and gross output
  X1 <- d1 %>%
    dplyr::filter(
      country==ctry, year==yr
    ) %>%
    dplyr::select("code","GO") %>%
    as.data.frame()
  
  # --- Identifying industries which need to be dropped
  # 1. Industries with zero gross output
  # 2. Industries that are fictitious
  
  # Industries with zero gross output
  ind_zero <- dput(X1[X1$GO==0,1])
  
  # Industries which are not profit making
  ind_nonpvt <- c("O84","T","U")
  
  
  # ----- Gross output  vector
  
  # -- All industries 
  # (a) remove industries with zero gross output
  # (b) remove industries that are not private sector
  X <- X1 %>%
    dplyr::filter(
      !code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    as.data.frame()
  
  # Reorder sectors
  ind1 <- X$code
  
  # Correct order
  X_temp <- X %>%
    dplyr::slice(match(ind1, code))
  
  # Diagonal matrix with gross output vector
  X_diag <- as.matrix(diag(X_temp[,2]))
  
  
  # ---- Labor input vector (not corrected for complexity)
  # Unit: millions of hours
  L1_h <- d1 %>%
    dplyr::filter(
      country==ctry, year==yr
    ) %>%
    dplyr::select("code","H_EMPE") %>%
    as.data.frame()
  
  # -- All industries 
  # (a) remove industries with zero gross output
  # (b) remove industries that are not private sector
  L2_h <- L1_h %>%
    dplyr::filter(
      !code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    as.data.frame()
  
  # Correct order of sectors
  Lh_temp <- L2_h %>%
    dplyr::slice(match(ind1, code))
  
  # ---- Labor input vector (corrected for complexity)
  L1 <- d1 %>%
    dplyr::filter(
      country==ctry, year==yr
    ) %>%
    dplyr::select("code","HRS2") %>%
    as.data.frame()
  
  # -- All industries 
  # (a) remove industries with zero gross output
  # (b) remove industries that are not private sector
  L2 <- L1 %>%
    dplyr::filter(
      !code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    as.data.frame()
  
  # Correct order of sectors
  L_temp <- L2 %>%
    dplyr::slice(match(ind1, code))
  
  
  # ------ Vector of wage rate
  W1 <- d1 %>%
    dplyr::filter(
      country==ctry, year==yr
    ) %>%
    dplyr::select("code","WAGE") %>%
    as.data.frame()
  
  # -- All industries 
  # (a) remove industries with zero gross output
  # (b) remove industries that are not private sector
  W2 <- W1 %>%
    dplyr::filter(
      !code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    as.data.frame()
  
  # Reorder sectors
  W_temp <- W2 %>%
    dplyr::slice(match(ind1, code))
  
  W <- W_temp[,2]
  
  # Average nominal wage rate
  w_avg <- mean(W_temp[,2], na.rm = TRUE)
  
  # ----- Value of labor power (average wage share)
  # For all sectors
  V1 <- d1 %>%
    dplyr::filter(
      country==ctry, year==yr
    ) %>%
    dplyr::select("code","WSHARE") %>%
    dplyr::filter(
      !code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    as.data.frame()
  
  V <- mean(V1[,2], na.rm=TRUE)
  
  
  # Profit share
  PS1 <- d1 %>%
    dplyr::filter(
      country==ctry, year==yr
    ) %>%
    dplyr::select("code","PSHARE_TOTAL") %>%
    dplyr::filter(
      !code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    as.data.frame()
  
  PS <- mean(PS1[,2], na.rm=TRUE)
  
  # ----------- Using IO Data --------------------- #
  
  # --- All sectors
  # Remove rows and columns for industries: 
  # (a) with zero gross output, (b) which are not profit making
  Z1 <- d2 %>%
    dplyr::filter(
      Year==yr
    ) %>%
    dplyr::filter(
      !Code %in% unique(c(ind_zero,ind_nonpvt))
    ) %>%
    dplyr::select(
      -all_of(unique(c(ind_zero,ind_nonpvt)))
    ) %>%
    as.data.frame()
  
  # Reorder rows
  Z2 <- Z1 %>%
    dplyr::slice(match(ind1, Code))
  
  # Reorder columns (correct order of rows and columns)
  Z3 <- Z2[,ind1]
  
  # Convert into matrix
  Z <- apply(as.matrix.noquote(Z3),2,as.numeric)
  
  # Input-output matrix (unitless)
  A_hat <- Z%*%solve(X_diag)
  
  # labor input vector (hours/USD)
  l_hat <- (matrix(data=Lh_temp[,2],nrow = 1) %*% solve(X_diag))
  
  # labor input vector adjusted for complexity
  l_hat_simple <- (matrix(data=L_temp[,2],nrow = 1) %*% solve(X_diag))
  
  # Gross output vector (million of USD) (use correct order: X_tem)
  Q <- matrix(data=X_temp[,2], ncol=1)
  
  # --- Consumption bundle (millions of USD)
  # Remove rows and columns for industries: 
  # (a) with zero gross output, (b) which are nonprivate sector
  d4 <- rwb %>%
    dplyr::filter(
      Year==yr
    ) %>%
    # remove rows
    dplyr::filter(
      !Code %in% ind_zero & !Code %in% ind_nonpvt
    ) %>%
    dplyr::select(-c("Code","Year")) %>%
    as.data.frame()
  
  # Convert into matrix
  d5 <- apply(as.matrix.noquote(d4),2,as.numeric)
  
  # Consumption bundle (USD/hour)
  b <-  matrix(data=d5,ncol = 1)/sum(Lh_temp[,2])
  
  
  # Return list of data objects
  return(
    list(
      "Ahat" = A_hat,
      "l" = l_hat,
      "l_simple" = l_hat_simple,
      "Q" = Q,
      "wavg" = w_avg,
      "wagevector_all" = as.vector(W),
      "vlp" = V,
      "b" = b,
      "pshare" = PS
    )
  )
  
}

