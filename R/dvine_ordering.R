#' Compute greedily an ordering based on Partial correlations
#'
#' The ordering might then be used to fit D vine copula models. If present
#' first the conditional variables are fixed at the beginning of the ordering
#' (as the ordering will be forwarded to e.g. `rvinecopulib::dvine_structure()`)
#' then always the variable with the biggest sum of partial correlations in
#' absolute terms respectively that correspond to the additional edges in the
#' D vine that come with the addition of a variable to the ordering.
#'
#' @param vine_train_data data.frame with copula data in the columns
#' @param cond_vars a character vector of columns that will be later used
#' conditionally. Currently available options are 1 or 2 column names or NULL.
#' @param cutoff_depth positive count that specifies the depth up to which the
#' edges of the to be constructed D-vine copula are considered in the algorithm
#' that determines the ordering using partial correlations. The default Inf
#' considers all edges and seems in most use cases reasonable.
#'
#' @return a numeric vector containing a permutation of the column indices
#'
#' @import dplyr
#' @importFrom stats cor
#' @importFrom stats qnorm
#' @noRd
dvine_ordering <- function(
  vine_train_data,
  cond_vars,
  cutoff_depth = Inf) {
  checkmate::assert_data_frame(vine_train_data,
    types = "numeric",
    any.missing = FALSE, min.cols = 3,
    col.names = "unique"
  )
  checkmate::assert_subset(cond_vars, colnames(vine_train_data))
  if (length(cond_vars) > 2) {
    stop("Conditioning on more than 2 variables is not yet implemented.")
  }
  # transform the copula data to the normalized scale
  vine_train_data <- vine_train_data  %>%
    mutate(across(everything(), qnorm))

  # pairwise pearson correlation coefficient
  pairwise_pearson <- cor(vine_train_data, method = "pearson")
  pairwise_pearson <- abs(pairwise_pearson[lower.tri(pairwise_pearson)])
  pairwise_pearson <- data.frame(
    pcoef = pairwise_pearson,
    ind2 = rep(
      1:(ncol(vine_train_data) - 1),
      rev(1:(ncol(vine_train_data) - 1))
    )
  ) %>%
    group_by(.data$ind2) %>%
    mutate(ind1 = seq(unique(.data$ind2) + 1, ncol(vine_train_data))) %>%
    ungroup() %>%
    mutate(id = seq.int(length(pairwise_pearson)))

  # prep the ordered / free_indices vectors according to the conditional setting
  if (length(cond_vars) == 0) {
    # unconditional case
    ordered_vec <- pairwise_pearson[["ind2"]][which.max(
      pairwise_pearson$pcoef)]
    free_indices  <- seq(ncol(vine_train_data))[-ordered_vec]
  } else if (length(cond_vars) == 1) {
    # one conditional variable
    ordered_vec <- which(colnames(vine_train_data) == cond_vars)
    free_indices  <- seq(ncol(vine_train_data))[-ordered_vec]
  } else if (length(cond_vars) == 2) {
    # two conditional variables
    cond_positions <- c(
      which(colnames(vine_train_data) == cond_vars[1]),
      which(colnames(vine_train_data) == cond_vars[2])
    )
    filtered_df <- pairwise_pearson %>%
      filter((.data$ind1 %in% cond_positions &
                !.data$ind2 %in% cond_positions) |
               (.data$ind2 %in% cond_positions &
                  !.data$ind1 %in% cond_positions))
    max_ind <- which.max(filtered_df$pcoef)
    ordered_vec <- ifelse(
      filtered_df$ind1[max_ind] %in% cond_positions,
      filtered_df$ind1[max_ind],
      filtered_df$ind2[max_ind]
    )
    ordered_vec <- c(
      ifelse(ordered_vec == cond_positions[1],
             cond_positions[2],
             cond_positions[1]),
      ordered_vec
    )
    ordered_vec <- c(
      ordered_vec,
      ifelse(filtered_df$ind1[max_ind] %in% cond_positions,
             filtered_df$ind2[max_ind],
             filtered_df$ind1[max_ind])
    )
    free_indices  <- seq(ncol(vine_train_data))[-ordered_vec]
    # possible early exit
    if (length(free_indices) == 0) return(ordered_vec)
  }
  while (length(free_indices) > 1) {
    argmax_index <- NULL
    argmax_value <- -Inf
    for (free_index in free_indices) {
      index_dependence <- 0
      for (depth in seq(min(cutoff_depth, length(ordered_vec)))) {
        # compute the necessary (partial) pearson correlation
        temp_dependence <- ppcor::pcor(
          vine_train_data[, c(free_index,
                              ordered_vec[seq(length(ordered_vec) + 1 - depth,
                                              length(ordered_vec))])]
        )$estimate[1, 2]
        index_dependence <- index_dependence + abs(temp_dependence)
      }
      if (index_dependence > argmax_value) {
        argmax_index <- free_index
        argmax_value <- index_dependence
      }
    }
    ordered_vec <- c(ordered_vec, argmax_index)
    free_indices <- free_indices[-which(free_indices == argmax_index)]
  }
  # append last free index
  c(ordered_vec, free_indices)
}
