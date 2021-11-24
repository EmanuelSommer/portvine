# data documentation ------------------------------------------------------

#' A sample of log returns for 3 assets.
#'
#' Data extracted from Yahoo Finance representing the daily og returns for
#' Google, Apple, and Amazon stocks between 2014-01-13 and 2018-01-01 which
#' results in exactly 1000 observations.
#'
#'
#'
#' @docType data
#'
#' @usage data(sample_returns_small)
#'
#' @format data.table with 3 columns and 1000 rows, columns `GOOG`, `AAPL`,
#'  `AMZN` contain the daily log return of the 3 stocks.
#'
#' @keywords datasets
#'
#' @source \href{https://finance.yahoo.com}{Yahoo Finance}
#'
#'
#' @examples
#' data(sample_returns_small)
#' head(sample_returns_small)
"sample_returns_small"


# for the RCPP functionality ----------------------------------------------

#' @useDynLib portvine, .registration = TRUE
#' @importFrom Rcpp sourceCpp
NULL

.onUnload <- function(libpath) {
  library.dynam.unload("portvine", libpath)
}
