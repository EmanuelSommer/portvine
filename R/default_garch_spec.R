
#' Default specifications for ARMA-GARCH models
#'
#' This function is used as the default for the univariate model fitting i.e.
#'  the marginal models and can be used to easily specify a different
#'  individual marginal model specification or default in
#'  [`marginal_settings()`].
#' The ARMA(`ar`,`ma`)-GARCH(`arch`,`garch`) is fitted  with the distribution
#'  given by `dist` that specifies the conditional density used for the
#'  innovations.
#'
#' @param ar integer for the autoregressive order
#' @param ma integer for the moving average order
#' @param arch integer for the ARCH order
#' @param garch integer for the GARCH order
#' @param dist a single character value of the possible distributions allowed in
#' [`rugarch::ugarchspec`]
#'
#' @return object of class [`rugarch::ugarchspec`]
#' @export
#'
#' @seealso [`marginal_settings()`]
#'
#' @examples # the default is then just using
#' default_garch_spec()
#' # to specify a ARMA(2,2)-GARCH(1,1) model with normal residual distribution
#' default_garch_spec(ar = 2, ma = 2, dist = "norm")
default_garch_spec <- function(ar = 1,
                               ma = 1,
                               arch = 1,
                               garch = 1,
                               dist = "sstd") {
  # basic input checks
  checkmate::assert_count(ar)
  checkmate::assert_count(ma)
  checkmate::assert_count(arch)
  checkmate::assert_count(garch)
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
