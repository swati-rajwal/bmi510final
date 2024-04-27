#' Maximizes log-likelihood for Bernoulli distribution
#'
#' @param data A numeric vector containing 0s and 1s.
#' @return The parameter p that maximizes the log-likelihood.
logLikBernoulli <- function(data) {
  p_grid <- seq(0, 1, by = 0.001)
  log_lik <- sapply(p_grid, function(p) sum(dbinom(data, size = 1, prob = p, log = TRUE)))
  max_p <- p_grid[which.max(log_lik)]
  return(max_p)
}

#' Calculates and plots a survival curve S(t)
#'
#' @param status Numerical vector indicating the status (1 = event, 0 = censored).
#' @param time Numerical vector of times at which the status is observed.
#' @return Plot of the survival curve.
# survCurv <- function(status, time) {
#   library(survival)
#   surv_obj <- Surv(time, status)
#   surv_fit <- survfit(surv_obj ~ 1)
#   plot(surv_fit, main = "Survival Curve", xlab = "Time", ylab = "Survival Probability")
#   return(invisible(surv_fit))
# }

survCurv <- function(status, time) {
  # Create the survival object
  surv_obj <- Surv(time, status)
  # Fit the survival model
  surv_fit <- survfit(surv_obj ~ 1)
  # Plot the survival curve
  plot(surv_fit, main = "Survival Curve", xlab = "Time", ylab = "Survival Probability")
  # The function does not need to return anything for plotting
}

#' Reverses the scaling and centering transformation applied to a vector
#'
#' @param x A scaled numeric vector.
#' @return The unscaled original vector.
unscale <- function(x) {
  orig_mean <- attr(x, "scaled:center")
  orig_sd <- attr(x, "scaled:scale")
  if (is.null(orig_mean)) orig_mean <- 0
  if (is.null(orig_sd)) orig_sd <- 1
  (x * orig_sd) + orig_mean
}

#' Principal Component Approximation of Data
#'
#' @param x A numeric matrix or data frame.
#' @param npc Number of principal components to use.
#' @return An approximation of the original data using the specified number of PCs.
pcApprox <- function(x, npc) {
  pca <- prcomp(x, scale. = TRUE)
  approx_data <- pca$x[, 1:npc] %*% t(pca$rotation[, 1:npc])
  scale_center <- attr(pca$scale, "scaled:center")
  scale_scale <- attr(pca$scale, "scaled:scale")
  approx_data <- sweep(approx_data, 2, scale_center, "+")
  sweep(approx_data, 2, scale_scale, "*")
}

#' Standardizes variable names in a data frame to small camel case
#'
#' @param data A tibble or data frame.
#' @return A tibble with standardized variable names.
standardizeNames <- function(data) {
  library(dplyr)
  library(janitor)
  library(snakecase)
  data %>%
    rename_with(~ make_clean_names(.) %>%
                  to_any_case(case = "small_camel"))
}