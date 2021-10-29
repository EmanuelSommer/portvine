#' Default specifications for ARMA-GARCH models
#'
#' This function is used as the default for the univariate model fitting i.e.
#'  the marginal models.
#' The ARMA(`ar`,`ma`)-GARCH(`arch`,`garch`) is fitted  with the distribution
#'  given by `dist` that specifies the conditional density used for the
#'  innovations.
#' It might also be a handy shortcut towards changing the default w.r.t. the main specification arguments.
#'
#' @param ar positive integer for the autoregressive order
#' @param ma positive integer for the moving average order
#' @param arch positive integer for the ARCH order
#' @param garch positive integer for the GARCH order
#' @param dist a single character value of the possible distributions allowed in
#' [`rugarch::ugarchspec`]
#'
#' @return object of class [`rugarch::ugarchspec`]
#' @export
#'
#' @examples # the default is then just using
#' default_garch_spec()
default_garch_spec <- function(
  ar = 1,
  ma = 1,
  arch = 1,
  garch = 1,
  dist = "sstd") {
  # basic input checks
  checkmate::assert_count(ar, positive = TRUE)
  checkmate::assert_count(ma, positive = TRUE)
  checkmate::assert_count(arch, positive = TRUE)
  checkmate::assert_count(garch, positive = TRUE)
  checkmate::assert_choice(dist, choices = c(
    "norm", "snorm", "std", "sstd", "ged", "sged", "nig", "jsu"
  ))
  # specify the result
  rugarch::ugarchspec(
    variance.model = list(garchOrder = c(arch, garch)),
    mean.model = list(armaOrder = c(ar, ma)),
    distribution.model = dist
  )
}
