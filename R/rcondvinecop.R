### First the functions that do the actual sampling and then the wrapper
### function rcondvinecop that enables the call of the correct sampling function

#' Sample conditioned on the rightmost edge variable from a D-vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_u a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param cond_pre_resid copula scale numeric value that corresponds to the
#' residual of the conditioning variable
#'  of one time unit before (For this conditional copula value one also
#' samples `n_samples`)
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha) + 1` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_u_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used (character) +
#'  entries "prior_resid" for the samples that use `cond_pre_resid`
#'
#' @noRd
r1conddvine <- function(n_samples, cond_u, cond_pre_resid, fitted_vine) {
  asset_names <- fitted_vine$names
  n_assets <- length(asset_names)
  # for each cond_alpha level get the n_samples samples
  sample_dt <- lapply(c(cond_u, cond_pre_resid), function(cond_alpha_level) {
    # call lower level cpp function for the actual sampling i.e. speed up
    # memory consumption down :)
    cond_dvine1_cpp(
      n_samples = n_samples,
      cond_u = cond_alpha_level,
      vinecop_r = fitted_vine
    ) %>%
      data.table::as.data.table()
  }) %>% data.table::rbindlist()

  # reorder the columns in the original order
  reorder_indices <- sapply(seq(asset_names), function(ind) {
    which(fitted_vine$structure$order == ind)
  }, simplify = TRUE)
  data.table::setcolorder(sample_dt, reorder_indices)
  colnames(sample_dt) <- asset_names

  list(
    sample_dt = sample_dt,
    cond_u_vec = rep(c(cond_u, "prior_resid"), each = n_samples)
  )
}



#' Sample conditioned on the two rightmost edge variables from a D-vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_u a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param cond_pre_resid copula scale numeric vector of length 2 that
#' corresponds to the
#' residuals of the conditional variables
#'  of one time unit before (For this conditional copula one also
#' samples `n_samples`) it should be named according to the assets
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha) + 1` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_u_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used (character) +
#'  entries "prior_resid" for the samples that use `cond_pre_resid`
#'
#' @noRd
r2conddvine <- function(n_samples, cond_u, cond_pre_resid, fitted_vine) {
  asset_names <- fitted_vine$names
  n_assets <- length(asset_names)
  cond_asset_names <- asset_names[fitted_vine$structure$order][1:2]

  # for each cond_alpha level get the n_samples samples
  sample_dt <- lapply(cond_u, function(cond_alpha_level) {
    # call lower level cpp function for the actual sampling i.e. speed up
    # memory consumption down :)
    cond_dvine2_cpp(
      n_samples = n_samples,
      cond_u1 = cond_alpha_level,
      cond_u2 = cond_alpha_level,
      quantile = TRUE,
      vinecop_r = fitted_vine
    ) %>%
      data.table::as.data.table()
  }) %>%
    data.table::rbindlist() %>%
    # samples for the prior conditional residuals
    rbind(
      cond_dvine2_cpp(
        n_samples = n_samples,
        cond_u1 = cond_pre_resid[cond_asset_names[2]],
        cond_u2 = cond_pre_resid[cond_asset_names[1]],
        quantile = FALSE,
        vinecop_r = fitted_vine
      ) %>%
        data.table::as.data.table()
    )

  # reorder the columns in the original order
  reorder_indices <- sapply(seq(asset_names), function(ind) {
    which(fitted_vine$structure$order == ind)
  }, simplify = TRUE)
  data.table::setcolorder(sample_dt, reorder_indices)
  colnames(sample_dt) <- asset_names

  list(
    sample_dt = sample_dt,
    cond_u_vec = rep(c(cond_u, "prior_resid"), each = n_samples)
  )
}

#' Sample conditionally from a vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_u a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param cond_pre_resid copula scale numeric vector of length `cond_vars` that
#' corresponds to the
#' residuals of the conditional variable(s)
#'  of one time unit before (For this conditional copula one also
#' samples `n_samples`) it should be named according to `cond_vars`
#' @param cond_vars character vector with the conditioning variables
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#' @param vine_type character vector specifing the vine copula type fitted. One
#' of `rvine` and `dvine` currently.
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha)` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_u_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used (character) +
#'  entries "prior_resid" for the samples that use `cond_pre_resid`
#'
#' @noRd
rcondvinecop <- function(n_samples, cond_u,
                         cond_pre_resid,
                         cond_vars,
                         fitted_vine, vine_type) {
  checkmate::assert_class(fitted_vine, "vinecop")
  checkmate::assert_numeric(cond_pre_resid,
    len = length(cond_vars),
    lower = 0, upper = 1, any.missing = FALSE,
    names = "unique"
  )
  checkmate::assert_subset(names(cond_pre_resid), cond_vars, empty.ok = FALSE)
  if (vine_type == "dvine") {
    if (length(cond_vars) == 1) {
      r1conddvine(
        n_samples = n_samples,
        cond_u = cond_u,
        cond_pre_resid = cond_pre_resid,
        fitted_vine = fitted_vine
      )
    } else if (length(cond_vars) == 2) {
      r2conddvine(
        n_samples = n_samples,
        cond_u = cond_u,
        cond_pre_resid = cond_pre_resid,
        fitted_vine = fitted_vine
      )
    } else {
      stop("Only the sampling of dvines with up to 2 conditioning variables
           is implemented yet.")
    }
  } else {
    stop("This vine type is not implemented for conditional sampling.")
  }
}
