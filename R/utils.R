#' Extract fitted standardized residuals from a uGARCHroll object
#'
#' The [`rugarch::ugarchroll`] class object encompasses fitting information
#' about a number of
#' models fitted in a rolling window fashion. This utility function gives an
#' easy interface to extract the fitted residuals from one of these models.
#' This can be especially helpful for assessing the model quality with a
#' residual analysis.
#'
#' @param ugarchroll Object of class [`rugarch::ugarchroll`].
#' @param roll_num Count that specifies the fitted model to extract the
#' residuals from.
#'
#' @return Numeric vector of the fitted standardized residuals.
#' @export
roll_residuals <- function(ugarchroll, roll_num = 1) {
  checkmate::assert_class(ugarchroll, classes = "uGARCHroll")
  total_roll_num <- ugarchroll@model$n.refits
  checkmate::assert_integerish(roll_num,
    lower = 0, upper = total_roll_num,
    len = 1
  )

  train_end_index <- ugarchroll@model$n.start
  refit_size <- ugarchroll@model$refit.every
  distribution <- ugarchroll@model$spec@model$modeldesc$distribution
  coefs <- ugarchroll@model$coef[[roll_num]]$coef[, 1]
  spec <- rugarch::ugarchspec(
    distribution.model = distribution,
    fixed.pars = coefs
  )
  filtered_model <- rugarch::ugarchfilter(
    spec = spec,
    data = ugarchroll@model$data[seq(
      1 + refit_size * (roll_num - 1),
      min(
        refit_size * (roll_num - 1) +
          train_end_index,
        length(ugarchroll@model$data)
      )
    )]
  )
  as.numeric(rugarch::residuals(filtered_model, standardize = TRUE))
}
