#' Socio Economic Accounts
#'
#' This is the socio economic accounts for the Australian economy extracted from the 2016 release of the World Input Output Database. It contains industry-level data on employment, capital stocks, gross output and value added at current and constant prices, in millions of local currency. The industry classification is consistent with the world input-output tables.
#'
#' @source \doi{10.34894/PJ2M1C}
#' @format A industry-level (53 industries) data set for Australia over 15 years, 2000-2014. 
#' \describe{
#' \item{country}{Country code.}
#' \item{code}{Industry code.}
#' \item{description}{Description of the industry.}
#' \item{variable}{One of the following variables:}
#' \item{GO}{Gross output by industry at current basic prices (in millions of national currency).}
#' \item{II}{Intermediate inputs at current purchasers' prices (in millions of national currency).}
#' \item{VA}{Gross value added at current basic prices (in millions of national currency).}
#' \item{EMP}{Number of persons engaged (thousands).}
#' \item{EMPE}{Number of employees (thousands).}
#' \item{H_EMPE}{Total hours worked by employees (millions).}
#' \item{COMP}{Compensation of employees (in millions of national currency).}
#' \item{LAB}{Labour compensation (in millions of national currency).}
#' \item{CAP}{Capital compensation (in millions of national currency).}
#' \item{K}{Nominal capital stock (in millions of national currency).}
#' \item{GO_PI}{Price levels gross output, 2010=100.}
#' \item{II_PI}{Price levels of intermediate inputs, 2010=100.}
#' \item{VA_PI}{Price levels of gross value added, 2010=100.}
#' \item{GO_QI}{Gross output, volume indices, 2010=100.}
#' \item{II_QI}{Intermediate inputs, volume indices, 2010=100.}
#' \item{VA_QI}{Value added, volume indices, 2010=100.}
#' \item{NOMEXCH}{Nominal exchange rate between the national currency and the US dollar.}
#' }
#' @examples
#' summary(aussea$COMP)
"aussea"