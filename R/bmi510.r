#' Maximizes log-likelihood for Bernoulli distribution
#'
#' @param data A numeric vector containing 0s and 1s.
#' @return The parameter p that maximizes the log-likelihood.
#' @export
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
#' @export
survCurv <- function(status, time) {
  ggsurvfit::survfit2(survival::Surv(time, status) ~ 1) |>
    ggsurvfit::ggsurvfit() +
    ggplot2::labs(
      x = "Time",
      y = "Overall survival probability"
    )
}

#' Reverses the scaling and centering transformation applied to a vector
#'
#' @param x A scaled numeric vector.
#' @return The unscaled original vector.
#' @export
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
#' @export
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
#' @export
standardizeNames <- function(data) {
  library(dplyr)
  library(janitor)
  library(snakecase)
  data %>%
    rename_with(~ make_clean_names(.) %>%
                  to_any_case(case = "small_camel"))
}

#' Calculate Minimum Sample Size for T-Test
#'
#' This function estimates the minimum sample size required for a t-test with
#' 80% power and a significance level of 0.05. It can perform calculations
#' for both one-sample and two-sample t-tests based on the input provided.
#'
#' @param x1 Numeric vector of preliminary data for the first sample or the only sample.
#' @param x2 Optional numeric vector of preliminary data for the second sample.
#' @param power The power for which the sample size is being calculated (default is 0.80).
#' @param sig_level The significance level to be used in the test (default is 0.05).
#'
#' @return An integer representing the minimum sample size needed to achieve the
#' specified power for a t-test.
#'
#' @export
#'
#' @examples
#' # For a one-sample t-test
#' data1 <- rnorm(10, mean = 5, sd = 1)
#' minimumN(data1)
#'
#' # For a two-sample t-test
#' data2 <- rnorm(10, mean = 5.5, sd = 1.2)
#' minimumN(data1, data2)
minimumN <- function(x1, x2 = NULL, power = 0.80, sig_level = 0.05) {
  if (!is.null(x2)) {
    # Two-sample t-test
    effect_size <- abs(mean(x1) - mean(x2)) / sqrt((var(x1)/length(x1)) + (var(x2)/length(x2)))
    n <- ceiling(pwr.t.test(
      n = NULL,
      d = effect_size,
      sig.level = sig_level,
      power = power,
      type = "two.sample"
    )$n)
  } else {
    # One-sample t-test
    effect_size <- (mean(x1) - 0) / sqrt(var(x1)/length(x1))
    n <- ceiling(pwr.t.test(
      n = NULL,
      d = effect_size,
      sig.level = sig_level,
      power = power,
      type = "one.sample"
    )$n)
  }
  return(n)
}


#' Download Report from REDCap
#'
#' Retrieves a specified report from REDCap using the API token, URL, and report ID provided.
#' The API token is sourced from the user's .Renviron file.
#'
#' @param redcapTokenName The name of the environment variable containing the REDCap API token.
#' @param redcapUrl The URL of the REDCap API.
#' @param redcapReportId The report ID number for the report you wish to download.
#'
#' @return A tibble containing the contents of the report.
#'
#' @export
#' @examples
#' # Example usage:
#' # downloadRedcapReport("REDCAP_API_TOKEN", "https://redcap.example.com/api/", "12345")
#'
#' # Note: Before using the function, make sure that your .Renviron file contains a line like:
#' # REDCAP_API_TOKEN="your_api_token_here"
downloadRedcapReport <- function(redcapTokenName, redcapUrl, redcapReportId) {
  # Load required libraries
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required but not installed.")
  }
  
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required but not installed.")
  }
  
  # Use Sys.getenv() to read the API token from the .Renviron file
  token <- Sys.getenv(redcapTokenName)
  print("token read!")
  
  # Check if the token is not empty
  if (token == "") {
    stop(paste("API token for", redcapTokenName, "not found in .Renviron file."))
  }
  
  # Prepare the body for the POST request
  formData <- list(
    token = token,
    content = 'report',
    format = 'csv',
    report_id = as.character(redcapReportId),
    csvDelimiter = '',
    rawOrLabel = 'raw',
    rawOrLabelHeaders = 'raw',
    exportCheckboxLabel = 'false',
    returnFormat = 'csv'
  )
  
  # Perform the POST request to download the report
  response <- httr::POST(redcapUrl, body = formData, encode = "form")
  result <- httr::content(response)
  return(tibble::tibble(result))

}
