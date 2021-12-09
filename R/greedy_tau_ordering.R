#' Compute greedily an ordering based on Kendall's tau
#'
#' The ordering might then be used to fit d or c vine copula models. If present
#' first the conditional variables are fixed at the beginning of the ordering
#' (as the ordering will be forwarded to e.g. `rvinecopulib::dvine_structure()`)
#' then always the variable with the highest pairwise Kendall's tau is appended.
#'
#' @param vine_train_data data.frame with copula data in the columns
#' @param cond_vars a character vector of columns that will be later used
#' conditionally. Currently available options are 1 or 2 column names or NULL.
#'
#' @return a numeric vector containing a permutation of the column indices
#'
#' @import dplyr
#' @importFrom stats cor
#' @noRd
greedy_tau_ordering <- function(
  vine_train_data,
  cond_vars
) {
  checkmate::assert_data_frame(vine_train_data, types = "numeric",
                               any.missing = FALSE, min.cols = 3,
                               col.names = "unique")
  checkmate::assert_subset(cond_vars, colnames(vine_train_data))
  if (length(cond_vars) > 2) {
    stop("Conditioning on more than 2 variables is not yet implemented.")
  }
  # get the absolute pairwise Kendall's tau
  pairwise_tau <- cor(vine_train_data, method = "kendall")
  pairwise_tau <- abs(pairwise_tau[lower.tri(pairwise_tau)])
  pairwise_tau_df <- data.frame(
    tau = pairwise_tau,
    ind2 = rep(1:(ncol(vine_train_data) - 1),
               rev(1:(ncol(vine_train_data) - 1)))
  ) %>%
    group_by(.data$ind2) %>%
    mutate(ind1 = seq(unique(.data$ind2) + 1, ncol(vine_train_data))) %>%
    ungroup() %>%
    mutate(id = seq.int(length(pairwise_tau)))
  # now determine the ordering
  order_vec <- rep(NA_real_, ncol(vine_train_data))
  if (length(cond_vars) == 0) {
    # just a greedy approach to get the max Kendall's tau
    max_ind <- which.max(pairwise_tau)
    order_vec[1] <- pairwise_tau_df$ind1[max_ind]
    order_vec[2] <- pairwise_tau_df$ind2[max_ind]
    pairwise_tau_df <- pairwise_tau_df[-max_ind, , drop = FALSE]
    for (i in 3:ncol(vine_train_data)) {
      filtered_df <- pairwise_tau_df %>%
        filter((.data$ind1 == order_vec[i - 1] &
                  !.data$ind2 %in% order_vec[seq.int(i - 2)]) |
                 (.data$ind2 == order_vec[i - 1] &
                    !.data$ind1 %in% order_vec[seq.int(i - 2)]))
      max_ind <- which.max(filtered_df$tau)
      order_vec[i] <- ifelse(filtered_df$ind1[max_ind] == order_vec[i - 1],
                             filtered_df$ind2[max_ind],
                             filtered_df$ind1[max_ind])
    }
  } else if (length(cond_vars) == 1) {
    # the position of the conditional variable is fixed
    order_vec[1] <- which(colnames(vine_train_data) == cond_vars)
    for (i in 2:ncol(vine_train_data)) {
      filtered_df <- pairwise_tau_df %>%
        filter((.data$ind1 == order_vec[i - 1] &
                  !.data$ind2 %in% order_vec[seq.int(i - 2)]) |
                 (.data$ind2 == order_vec[i - 1] &
                    !.data$ind1 %in% order_vec[seq.int(i - 2)]))
      max_ind <- which.max(filtered_df$tau)
      order_vec[i] <- ifelse(filtered_df$ind1[max_ind] == order_vec[i - 1],
                             filtered_df$ind2[max_ind],
                             filtered_df$ind1[max_ind])
    }
  } else if (length(cond_vars) == 2) {
    # determine first the highest Kendall's tau between a pair of variables
    # where one variable is a conditional variable
    cond_positions <- c(which(colnames(vine_train_data) == cond_vars[1]),
                        which(colnames(vine_train_data) == cond_vars[2]))
    filtered_df <- pairwise_tau_df %>%
      filter((.data$ind1 %in% cond_positions &
                !.data$ind2 %in% cond_positions) |
               (.data$ind2 %in% cond_positions &
                  !.data$ind1 %in% cond_positions))
    max_ind <- which.max(filtered_df$tau)
    order_vec[2] <- ifelse(filtered_df$ind1[max_ind] %in% cond_positions,
                           filtered_df$ind1[max_ind],
                           filtered_df$ind2[max_ind])
    order_vec[1] <- ifelse(order_vec[2] == cond_positions[1],
                           cond_positions[2],
                           cond_positions[1])
    order_vec[3] <- ifelse(filtered_df$ind1[max_ind] %in% cond_positions,
                           filtered_df$ind2[max_ind],
                           filtered_df$ind1[max_ind])
    if (ncol(vine_train_data) > 3) {
      for (i in 4:ncol(vine_train_data)) {
        filtered_df <- pairwise_tau_df %>%
          filter((.data$ind1 == order_vec[i - 1] &
                    !.data$ind2 %in% order_vec[seq.int(i - 2)]) |
                   (.data$ind2 == order_vec[i - 1] &
                      !.data$ind1 %in% order_vec[seq.int(i - 2)]))
        max_ind <- which.max(filtered_df$tau)
        order_vec[i] <- ifelse(filtered_df$ind1[max_ind] == order_vec[i - 1],
                               filtered_df$ind2[max_ind],
                               filtered_df$ind1[max_ind])
      }
    }
  }
  order_vec
}
