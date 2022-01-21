# estimate_risk_roll ------------------------------------------------------

#' (Un-)conditional rolling risk estimation using vine copulas
#'
#' TBD! Shortly discuss the overall algorithm but reference a hands on get
#'  started
#' vignette / theoretical vignette and the paper/ thesis
#'
#' TBD! Address all dependencies of the vine and marginal settings parameters
#' like n_all_obs - n_marg_train > n_marg_refit. i.e. there must be at least 2
#' marginal and thus also 2 vine windows. Maybe in a table (markdown) give short
#' notice about availble risk_measures
#'
#' @section Parallel processing:
#' This function uses the [`future`](https://www.futureverse.org/)
#' framework for parallelization that allows maximum flexibility for the user
#' while having safe speedups for example regarding random number generation.
#' The default is of course the standard non parallel sequential evaluation.
#' The user has to do nothing in order for this default to work. If the user
#' wants to run the code in parallel there are many options from parallel on a
#' single machine up to a high performance compute (HPC) cluster, all of this
#' with just one setting switch i.e. by calling the function [`future::plan()`]
#' with the respective argument before the function call. Common options are
#' `future::plan("multisession")` which works on all major operating systems
#' and uses all available cores to run the code in parallel local R sessions.
#' To specify the number of workers use
#' `future::plan("multisession", workers = 2)`. To go back to sequential
#'  processing and to shut down the parallel sessions use
#'  `future::plan("sequential")`.
#'  For more information have a look at [`future::plan()`]. The two following
#'  loops are processed in parallel by default if a parallel [`future::plan()`]
#'   is set:
#'  - The marginal model fitting i.e. all assets individually in parallel.
#'  - The vine windows i.e. the risk estimates and the corresponding vine copula
#'  models are computed in parallel for each rolling vine window.
#' In addition the function allows for nested parallelization which has to
#' be done with care. So in addition to the 2 loops above one can further
#' run each computation for each time unit in the vine windows in parallel which
#' might be especially interesting if the `n_samples` argument is large. Then
#' the default parallelization has to be tweaked to not only parallelize the
#' first level of parallelization which are the 2 loops above. This can be
#' achieved e.g. via `future::plan(list(future::tweak(future::multisession,
#' workers = 4), future::tweak(future::multisession, workers = 2)))`. This
#' setting would run the 2 primary loops in 4 parallel R sessions and in
#' addition each of the 4 primary parallel sessions would itself use 2 sessions
#' within the nested parallel loop over the time units in the vine window. This
#' results in a need for at least 2 times 4 so 8 threads on the hardware side.
#' More details can be found in the extensive documentation of the
#' [`future`](https://www.futureverse.org/) framework.
#'
#' @param data Matrix, data.frame or other object coercible to a data.table
#'  storing the numeric asset returns in the named columns (at least 3).
#'  Moreover missing values must be imputed beforehand.
#' @param weights Corresponding named non-negative weights of the assets
#'  (conditioning variables must have weight 0). Default `NULL` gives equal
#'  weight to each non conditional asset. Alternatively one can use a matrix
#'  with as many rows as vine windows for changing weights. The matrix must have
#'  column names corresponding to the assets and conditional assets have to have
#'  weight 0.
#' @param marginal_settings [`marginal_settings`] S4 object containing the
#'  needed information for the ARMA-GARCH i.e. marginal models fitting. Note
#'  that the `marginal_settings` and `vine_settings` objects have to match as
#'  described further below.
#' @param vine_settings [`vine_settings`] S4 object containing the
#'  needed information for the vine copula model fitting. Note
#'  that the `marginal_settings` and `vine_settings` objects have to match as
#'  described further below.
#' @param alpha Numeric vector specifying the confidence levels in (0,1) at
#' which the risk measures should be calculated.
#' @param risk_measures Character vector with valid choices for risk
#'  measures to compute. Currently available are the Value at Risk `VaR` which
#'  is implemented in [`est_var()`] and 3 estimation methods of the Expected
#'  Shortfall `ES_mean`, `ES_median` and `ES_mc` all implemented in [`est_es()`]
#'  .
#' @param n_samples Positive count of samples to be used at the base of the risk
#'  measure estimation.
#' @param cond_vars Names of the variables to sample conditionally from
#' (currently \eqn{\le 2} variables).
#' @param cond_u Numeric vector specifying the corresponding quantiles
#'  in (0,1) of the conditional variable(s) conditioned on which the conditional
#'  risk measures should be calculated. Additionally always the conditioning
#'  values corresponding to the residual of one time unit prior are used as
#'  conditional variables.
#' @param n_mc_samples Positive count of samples for the Monte Carlo integration
#' if the risk measure `ES_mc` is used. (See [`est_es()`])
#' @param trace If set to TRUE the algorithm will print basic information while
#'  running.
#'
#' @return In the unconditional case an S4 object of class `portvine_roll` and
#' in the conditional case its child class `cond_portvine_roll`. For details
#' see [`portvine_roll-class`].
#' @export
#'
#' @seealso [`portvine_roll-class`], [`marginal_settings`], [`vine_settings`],
#'  [`est_var()`], [`est_es()`]
#'
#' @author Emanuel Sommer
#'
#' @importFrom rlang .data
#' @importFrom tidyr pivot_longer pivot_wider
#' @import dplyr
#' @rawNamespace import(data.table, except = c(first,last,between))
#'
#' @examples # TBD
estimate_risk_roll <- function(data,
                               weights = NULL,
                               marginal_settings,
                               vine_settings,
                               alpha = 0.05,
                               risk_measures = c("VaR", "ES_mean"),
                               n_samples = 1000,
                               cond_vars = NULL,
                               cond_u = 0.05,
                               n_mc_samples = 1000,
                               trace = FALSE) {
  # Return also the total run time at the end
  start_time <- Sys.time()
  # Input checks ----------------------------------------------------------
  # data argument
  data <- try(data.table::as.data.table(data), silent = TRUE)
  if ("try-error" %in% class(data)) {
    stop("The <data> argument is not coercible to a data.table.")
  }
  checkmate::assert_data_table(data,
    any.missing = FALSE, min.cols = 3,
    types = "numeric", col.names = "unique"
  )
  # cond_vars argument
  all_asset_names <- colnames(data)
  checkmate::assert_character(cond_vars,
    any.missing = FALSE, max.len = 2,
    null.ok = TRUE
  )
  checkmate::assert_subset(cond_vars, all_asset_names)
  conditional_logical <- ifelse(is.null(cond_vars), FALSE, TRUE)

  # alpha argument
  checkmate::assert_numeric(alpha,
    lower = 0, upper = 1, any.missing = FALSE,
    min.len = 1
  )
  # risk_measures argument
  checkmate::assert_subset(risk_measures, c(
    "VaR", "ES_mean",
    "ES_median", "ES_mc"
  ))
  # n_samples, cond_u
  checkmate::assert_integerish(n_samples, lower = 1)
  checkmate::assert_numeric(cond_u,
    lower = 0, upper = 1,
    any.missing = FALSE, min.len = 1
  )
  # marginal_settings and vine_settings
  checkmate::assert_class(marginal_settings, "marginal_settings")
  checkmate::assert_class(vine_settings, "vine_settings")
  n_all_obs <- nrow(data)
  n_marg_train <- marginal_settings@train_size
  n_marg_refit <- marginal_settings@refit_size
  n_vine_train <- vine_settings@train_size
  n_vine_refit <- vine_settings@refit_size
  vine_family_set <- vine_settings@family_set
  if (conditional_logical & any(vine_family_set %in% c("all", "tll"))) {
    stop("Nonparametric vine copula classes are not supported for conditional
         sampling.")
  }
  vine_type <- vine_settings@vine_type
  # check whether the vine type is implemented for the conditional sampling
  if (conditional_logical & vine_type == "rvine") {
    stop(paste("For vine type", vine_type, "the conditional sampling is not
               implemented yet."))
  }
  # check whether the individual settings are a valid match
  if (n_marg_train >= n_all_obs |
    n_marg_refit >= n_all_obs - n_marg_train |
    n_vine_train > n_marg_train |
    n_vine_refit > n_marg_refit |
    n_marg_refit %% n_vine_refit != 0) {
    stop("The train and refit sizes of the marginal and vine settings are
         not a valid combination.
         Have a look at the help page for details.")
  }
  if ((n_all_obs - n_marg_train) %% n_marg_refit != 0) {
    message(paste(
      "The last window of interest is shorter (width: ",
      (n_all_obs - n_marg_train) %% n_marg_refit,
      ") than the specified window width of",
      n_marg_refit
    ))
  }
  # weights argument
  n_all_assets <- length(all_asset_names)
  if (!is.matrix(weights) & !is.null(weights)) {
    weights <- try(
      {
        # try to coerce vector to matrix that replicates the vector in each row
        # for a number of the vine windows
        matrix(
          rep(weights, ceiling((n_all_obs - n_marg_train) / n_vine_refit)),
          byrow = TRUE,
          ncol = length(weights),
          nrow = ceiling((n_all_obs - n_marg_train) / n_vine_refit),
          dimnames = list(NULL, names(weights))
        )
      },
      silent = TRUE
    )
    if ("try-error" %in% class(weights)) {
      stop("The <weights> argument is not specified correctly.
           Should be a named numeric vector or matrix.")
    }
  }
  checkmate::assert_matrix(
    weights,
    mode = "numeric", any.missing = FALSE,
    nrows = ceiling((n_all_obs - n_marg_train) / n_vine_refit),
    ncols = n_all_assets, col.names = "unique", null.ok = TRUE
  )
  checkmate::assert_subset(colnames(weights), all_asset_names)
  if (any(weights[, cond_vars] != 0) & conditional_logical) {
    stop("The weights of the conditioning variables must be 0.")
  }
  if (any(weights < 0)) {
    stop("Negative weights are not supported")
  }
  # prep the weights
  if (is.null(weights)) {
    # if not specified all assets get equal weight
    weights <- matrix(
      rep(1, n_all_assets * ceiling((n_all_obs - n_marg_train) / n_vine_refit)),
      ncol = n_all_assets
    )
    colnames(weights) <- all_asset_names
    # conditioning variables get weight 0
    if (conditional_logical) {
      weights[, cond_vars] <- 0
    }
  }
  # trace argument
  checkmate::assert_flag(trace)


  # Preparations for the overall algorithm -------------------------------

  # add id column and get into long format
  data <- dtplyr::lazy_dt(data) %>%
    mutate(row_num = seq.int(nrow(data))) %>%
    pivot_longer(-"row_num", names_to = "asset", values_to = "returns") %>%
    data.table::as.data.table()

  # prep the marginal specifications for each asset
  marginal_specs_list <- sapply(all_asset_names, function(asset) {
    if (asset %in% names(marginal_settings@individual_spec)) {
      marginal_settings@individual_spec[[asset]]
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
    cond_u = cond_u,
    n_mc_samples = n_mc_samples,
    trace = trace
  )

  # compute the realized portfolio returns and left join them to the estimated
  # risk measures (if conditional do it also for the conditional estimates)
  realized_portfolio_returns <- data %>%
    dtplyr::lazy_dt() %>%
    filter(.data$row_num > n_marg_train) %>%
    group_by(.data$asset) %>%
    mutate(auxiliary_rowid = seq(n())) %>%
    ungroup() %>%
    mutate(vine_window = ceiling(.data$auxiliary_rowid / n_vine_refit)) %>%
    group_by(.data$asset, .data$vine_window) %>%
    mutate(weight = weights[.data$vine_window, .data$asset]) %>%
    ungroup() %>%
    group_by(.data$row_num) %>%
    summarise(
      realized = sum(.data$weight * .data$returns),
      .groups = "drop"
    ) %>%
    data.table::as.data.table()
  risk_estimates <- dep_risk_result[["overall_risk_estimates"]] %>%
    dtplyr::lazy_dt() %>%
    left_join(realized_portfolio_returns, by = "row_num") %>%
    data.table::as.data.table()
  if (conditional_logical) {
    cond_risk_estimates <- dep_risk_result[["cond_risk_estimates"]] %>%
      dtplyr::lazy_dt() %>%
      left_join(realized_portfolio_returns, by = "row_num") %>%
      data.table::as.data.table()
  }

  end_time <- Sys.time()
  time_taken_minutes <- as.numeric(
    difftime(end_time, start_time),
    units = "mins"
  )

  # return the correct output class
  if (!conditional_logical) {
    methods::new("portvine_roll",
      risk_estimates = risk_estimates,
      fitted_marginals = lapply(
        marg_mod_result,
        function(asset) asset$roll_model_fit
      ),
      fitted_vines = dep_risk_result[["fitted_vines"]],
      marginal_settings = marginal_settings,
      vine_settings = vine_settings,
      risk_measures = risk_measures,
      alpha = alpha,
      weights = weights,
      cond_estimation = conditional_logical,
      n_samples = n_samples,
      time_taken = time_taken_minutes
    )
  } else {
    methods::new("cond_portvine_roll",
      risk_estimates = risk_estimates,
      fitted_marginals = lapply(
        marg_mod_result,
        function(asset) asset$roll_model_fit
      ),
      fitted_vines = dep_risk_result[["fitted_vines"]],
      marginal_settings = marginal_settings,
      vine_settings = vine_settings,
      risk_measures = risk_measures,
      alpha = alpha,
      weights = weights,
      cond_estimation = conditional_logical,
      n_samples = n_samples,
      time_taken = time_taken_minutes,
      cond_risk_estimates = cond_risk_estimates,
      cond_vars = cond_vars,
      cond_u = cond_u
    )
  }
}
