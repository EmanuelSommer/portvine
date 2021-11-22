### First the functions that do the actual sampling and then the wrapper
### function rcondvinecop that enables the call of the correct sampling function

#' Sample conditioned on the rightmost edge variable from a D-vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha)` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_alpha_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used.
#'
#' @noRd
r1conddvine <- function(n_samples, cond_alpha, fitted_vine){
  asset_names <- fitted_vine$names
  n_assets <- length(asset_names)
  # for each cond_alpha level get the n_samples samples
  sample_dt <- lapply(cond_alpha, function(cond_alpha_level) {
    replicate(n_samples, {
      aux_matrix <- matrix(data = NA_real_,
                           nrow = n_assets,
                           ncol = n_assets)
      # initialize the diagonal with the conditional value as well as the
      # sampled values
      diag(aux_matrix) <- c(cond_alpha_level, stats::runif(n_assets - 1))

      for (j in 2:n_assets) {
        for (k in (j - 1):1) {
          current_bicop <- fitted_vine$pair_copulas[[k]][[j - k]]
          aux_matrix[k, j] <- rvinecopulib:::bicop_hinv2_cpp(
            u = matrix(
              c(aux_matrix[k + 1, j], aux_matrix[k, j - 1]),
              ncol = 2
            ),
            bicop_r = current_bicop
          )
          if (j < n_assets) {
            aux_matrix[k + 1, j] <- rvinecopulib:::bicop_hfunc2_cpp(
              u = matrix(
                c(aux_matrix[k, j - 1], aux_matrix[k, j]),
                ncol = 2
              ),
              bicop_r = current_bicop
            )
          }
        }
      }
      # return the first row containing the multivariate conditional sample with
      # the conditioning value
      aux_matrix[1, ]
    }) %>%
      t() %>%
      data.table::as.data.table()
  }) %>% data.table::rbindlist()

  # reorder the columns in the original order
  reorder_indices <- sapply(seq(asset_names), function(ind) {
    which(fitted_vine$structure$order == ind)}, simplify = TRUE)
  data.table::setcolorder(sample_dt, reorder_indices)
  colnames(sample_dt) <- asset_names

  list(sample_dt = sample_dt,
       cond_alpha_vec = sample_dt[[fitted_vine$structure$order[1]]])
}



#' Sample conditioned on the two rightmost edge variables from a D-vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
#' @param fitted_vine object of class  [`rvinecopulib::vinecop`] containing all
#' the information about the fitted vine copula
#'
#' @return list with 2 entries:
#' - `sample_dt` data.table with `n_samples` times `length(cond_alpha)` rows
#'  each corresponding to one multivariate sample, each column is one of the
#' assets (named)
#' - `cond_alpha_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used.
#'
#' @noRd
r2conddvine <- function(n_samples, cond_alpha, fitted_vine) {
  asset_names <- fitted_vine$names
  n_assets <- length(asset_names)
  # the cond_alpha value will be set for the second rightmost value (the market
  #index with the strongest relationship to the assets) as the copula scale
  # conditional value. To mimic the cond_alpha level quantile of both
  # conditional variables the conditional value of the rightmost variable will
  # be computed based on the copula between the two variables
  cond_alpha_second <- rvinecopulib:::bicop_hinv2_cpp(
    u = matrix(
      c(cond_alpha, cond_alpha),
      ncol = 2, byrow = FALSE,
    ),
    bicop_r = fitted_vine$pair_copulas[[1]][[1]]
  )
  # for each cond_alpha level get the n_samples samples
  sample_dt <- lapply(seq(length(cond_alpha)), function(cond_alpha_ind) {
    replicate(n_samples, {
      aux_matrix <- matrix(data = NA_real_,
                           nrow = n_assets,
                           ncol = n_assets)
      # initialize the diagonal/first two entries of the first row with the
      # conditional values as well as the sampled values. Note that the value
      # 2,2 of the auxiliary matrix is calculated as it will be used to
      # compute the the first conditional asset sample
      aux_matrix[1, 2] <- cond_alpha_second[cond_alpha_ind]

      diag(aux_matrix) <- c(
        cond_alpha[cond_alpha_ind],
        rvinecopulib:::bicop_hfunc2_cpp(
          u = matrix(
            c(cond_alpha[cond_alpha_ind], cond_alpha_second[cond_alpha_ind]),
            ncol = 2, byrow = FALSE,
          ),
          bicop_r = fitted_vine$pair_copulas[[1]][[1]]
        ),
        stats::runif(n_assets - 2)
      )

      for (j in 3:n_assets) {
        for (k in (j - 1):1) {
          current_bicop <- fitted_vine$pair_copulas[[k]][[j - k]]
          aux_matrix[k, j] <- rvinecopulib:::bicop_hinv2_cpp(
            u = matrix(
              c(aux_matrix[k + 1, j], aux_matrix[k, j - 1]),
              ncol = 2
            ),
            bicop_r = current_bicop
          )
          if (j < n_assets) {
            aux_matrix[k + 1, j] <- rvinecopulib:::bicop_hfunc2_cpp(
              u = matrix(
                c(aux_matrix[k, j - 1], aux_matrix[k, j]),
                ncol = 2
              ),
              bicop_r = current_bicop
            )
          }
        }
      }
      # return the first row containing the multivariate conditional sample with
      # the conditioning value
      aux_matrix[1, ]
    }) %>%
      t() %>%
      data.table::as.data.table()
  }) %>% data.table::rbindlist()

  # reorder the columns in the original order
  reorder_indices <- sapply(seq(asset_names), function(ind) {
    which(fitted_vine$structure$order == ind)}, simplify = TRUE)
  data.table::setcolorder(sample_dt, reorder_indices)
  colnames(sample_dt) <- asset_names

  list(sample_dt = sample_dt,
       cond_alpha_vec = sample_dt[[fitted_vine$structure$order[1]]])
}


#' Sample conditionally from a vine copula
#'
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param cond_alpha a numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated.
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
#' - `cond_alpha_vec` dimensions s.t. it would fit as an extra column to
#'  `sample_dt` indicating the level of `cond_alpha` used.
#'
#' @noRd
rcondvinecop <- function(
  n_samples, cond_alpha,
  cond_vars,
  fitted_vine, vine_type
) {
  checkmate::assert_class(fitted_vine, "vinecop")
  if (vine_type == "dvine") {
    if (length(cond_vars) == 1) {
      r1conddvine(n_samples, cond_alpha, fitted_vine)
    } else if ( length(cond_vars) == 2) {
      r2conddvine(n_samples, cond_alpha, fitted_vine)
    } else {
      stop("Only the sampling of dvines with up to 2 conditioning variables
           is implemented yet.")
    }
  } else {
    stop("This vine type is not implemented for conditional sampling.")
  }
}
