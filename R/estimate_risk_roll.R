
#' TBD
#'
#' @param data matrix, data.frame or other object coercible to a data.table
#'  storing the numeric asset returns in the named columns (at least 3).
#'  Moreover missing values must be imputed beforehand.
#' @param weights corresponding named non-negative weights of the assets
#'  (conditioning variables must have weight 0).
#' @param marginal_settings containing the needed information for the ARMA-GARCH
#'  fitting i.e. marginal models.
#' @param vine_settings containing needed information for the vine fitting
#' @param alpha a numeric vector specifying the levels in (0,1) at which the
#' risk measures should be calculated
#' @param risk_measures a character vector with valid choices for risk
#'  measures to compute
#' @param cond_vars colnames of the variables to sample conditionally from
#' (currently \eqn{\leq 2})
#' @param n_samples number of samples to compute for the risk measure estimates
#' @param n_cond_samples number of samples of the conditioning variables
#' @param n_mc_samples number of samples for the Monte Carlo integration
#' if the risk measure `ES_mc` is used. (See [`est_es()`])
#' @param trace if set to TRUE the algorithm will print information while
#'  running.
#'
#' @return risk_roll class should be specified + link to doc
#' @export
#'
#' @importFrom dtplyr lazy_dt
#' @importFrom tidyr pivot_longer pivot_wider
#' @import dplyr
#' @rawNamespace import(data.table, except = c(first,last,between))
#'
#' @examples #TBD
estimate_risk_roll <- function(
  data,
  weights = NULL,
  marginal_settings,
  vine_settings,
  alpha = 0.05,
  risk_measures = c("VaR", "ES_mean"),
  cond_vars = NULL,
  n_samples = 1000,
  n_cond_samples = 100,
  n_mc_samples = 1000,
  trace = TRUE
) {
  # Return also the total run time at the end
  start_time <- Sys.time()
  # Input checks ----------------------------------------------------------
  # data argument
  data <- try(data.table::as.data.table(data), silent = TRUE)
  if ("try-error" %in% class(data)) {
    stop("The <data> argument is not coercible to a data.table.")
  }
  checkmate::assert_data_table(data, any.missing = FALSE, min.cols = 3,
                               types = "numeric", col.names = "unique")
  # cond_vars argument
  all_asset_names <- colnames(data)
  checkmate::assert_character(cond_vars, any.missing = FALSE, max.len = 2,
                              null.ok = TRUE)
  checkmate::assert_subset(cond_vars, all_asset_names)
  conditional_logical <- ifelse(is.null(cond_vars), FALSE, TRUE)
  # weights argument
  n_all_assets <- length(all_asset_names)
  checkmate::assert_numeric(weights, lower = 0, any.missing = FALSE,
                            len = n_all_assets, names = "unique",
                            null.ok = TRUE)
  checkmate::assert_subset(names(weights), all_asset_names)
  if (any(weights[cond_vars] != 0 & conditional_logical)) {
    stop("The weights of the conditioning variables must be 0.")
  }
  # alpha argument
  checkmate::assert_numeric(alpha, lower = 0, upper = 1, any.missing = FALSE,
                            min.len = 1)
  # risk_measures argument
  checkmate::assert_subset(risk_measures, c("VaR", "ES_mean",
                                            "ES_median", "ES_mc"))
  # n_samples, n_cond_samples, seed arguments
  checkmate::assert_integerish(n_samples, lower = 1)
  checkmate::assert_integerish(n_cond_samples, lower = 1)
  checkmate::assert_integerish(seed, lower = 1)
  # marginal_settings and vine_settings
  checkmate::assert_class(marginal_settings, "marginal_settings")
  checkmate::assert_class(vine_settings, "vine_settings")
  n_all_obs <- nrow(data)
  n_marg_train <- marginal_settings@train_size
  n_marg_refit <- marginal_settings@refit_size
  n_vine_train <- vine_settings@train_size
  n_vine_refit <- vine_settings@refit_size
  vine_family_set <- vine_settings@family_set
  vine_type <- vine_settings@vine_type
  # check whether the vine type is implemented for the conditional sampling
  if (conditional_logical & vine_type == "rvine") {
    stop(paste("For vine type", vine_type, "the conditional sampling is not implemented yet."))
  }
  # check whether the individual settings are a valid match
  if (n_marg_train >= n_all_obs |
      n_marg_refit > n_all_obs - n_marg_train |
      n_vine_train > n_marg_train |
      n_vine_refit > n_marg_refit |
      n_marg_refit %% n_vine_refit != 0) {
    stop("The train and refit sizes of the marginal and vine settings are not a valid combination.")
  }
  if ((n_all_obs - n_marg_train) %% n_marg_refit != 0) {
    message(paste(
      "The last window of interest is shorter (width: ",
      (n_all_obs - n_marg_train) %% n_marg_refit,
      ") than the specified window width of",
      n_marg_refit))
  }
  # trace argument
  checkmate::assert_flag(trace)


  # Preparations for the overall algorithm -------------------------------

  # add id column and get into long format
  data <- lazy_dt(data) %>%
    mutate(row_num = seq.int(nrow(data))) %>%
    pivot_longer(-row_num, names_to = "asset", values_to = "returns") %>%
    data.table::as.data.table()
  # prep the weights
  if (is.null(weights)) {
    # if not specified all assets get equal weight
    weights <- rep(1, n_all_assets)
    names(weights) <- all_asset_names
    # conditioning variables get weight 0
    if (conditional_logical) {
      weights[cond_vars] <- 0
    }
  }
  # prep the marginal specifications for each asset
  marginal_specs_list <- sapply(all_asset_names, function(asset) {
    if (asset %in% names(marginal_settings@indicidual_spec)) {
      marginal_settings@indicidual_spec[[asset]]
    } else {
      marginal_settings@default_spec
    }
  }, simplify = FALSE, USE.NAMES = TRUE)

  # Estimate the marginal models in a rolling window fashion -------------
  marg_mod_result <- estimate_marginal_models(
    data = data,
    n_all_obs = n_all_obs,
    n_marg_train = n_marg_train, n_marg_refit = n_marg_refit,
    n_vine_train = n_vine_train,
    all_asset_names = all_asset_names,
    marginal_specs_list = marginal_specs_list,
    trace = trace
  )
  # extract and combine all the estimated copula data into one data.table
  combined_residuals_dt <- data.table::rbindlist(
    lapply(marg_mod_result, function(asset) asset$residuals_dt)
  )

  # Estimate the dependence structure and the risk measures --------------
  # by simulation in a rolling window fashion ----------------------------
  dep_risk_result <- estimate_dependence_and_risk(
    combined_residuals_dt = combined_residuals_dt,
    n_all_obs = n_all_obs,
    n_marg_train = n_marg_train, n_marg_refit = n_marg_refit,
    n_vine_train = n_vine_train, n_vine_refit = n_vine_refit,
    all_asset_names = all_asset_names,
    family_set = vine_family_set, vine_type = vine_type,
    alpha = alpha,
    risk_measures = risk_measures,
    weights = weights,
    cond_vars = cond_vars,
    n_samples = n_samples,
    n_cond_samples = n_cond_samples,
    n_mc_samples = n_mc_samples,
    trace = trace
  )

  # the output S4 class will contain:
  # time_taken
  # roll_model_fit (ARMAGARCH)
  # fitted_vines list
  # risk_estimates
  # boolean for conditional or not
  # data ? keep data argument?
  # marginal settins
  # vine settings
  # risk_measures
  # alpha
  # n_samples
  # weights
  # ???? as.datatable argument else output dataframe?

  # child class with cond_risk_estimates
  # cond vars
  # n_cond_samples

  # print and summary methods
  # no constructor needed
  # just very basic validity function
  # no prototype

  end_time <- Sys.time()
  time_taken <- end_time - start_time
}






