#' Question 1: R function to Maximize log-likelihood for Bernoulli distribution
#'
#' @param data Numeric vector containing 0s and 1s.
#' @return The parameter p that maximizes the log-likelihood.
#' @export
#' @examples
#' print(logLikBernoulli(c(1, 0, 0, 0, 1, 1, 1)))
logLikBernoulli = function(data) {
  p_grid = seq(0, 1, by = 0.001)
  log_likelihoods = numeric(length(p_grid))
  for (i in seq_along(p_grid)) {
    p = p_grid[i]
    log_likelihoods[i] = sum(dbinom(data, size = 1, prob = p, log = TRUE))
  }
  max_index = which.max(log_likelihoods)
  max_p =p_grid[max_index]
  max_p
}

#' Question 2: R function to calculate and plot a survival curve S(t)
#'
#' @param status Numerical vector indicating the status (1 = event, 0 = censored).
#' @param time Numerical vector of times at which the status is observed.
#' @return Plot of the survival curve.
#' @export
#' @examples
#' # Load the survival dataset
#' survival_url = "https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv"
#' survival_data = read.csv(survival_url)
#' # Calculate and plot the survival curve
#' survCurv(survival_data$status, survival_data$time)
survCurv = function(status, time) {
  data = data.frame(time = time, status = status)
  data = data[order(data$time),]
  summary_data = aggregate(status ~ time, data = data, FUN = function(x) sum(x == 1))
  names(summary_data) = c("time", "n_events")
  summary_data$n_risk = rev(cumsum(rev(summary_data$n_events)))
  summary_data$n_risk = max(summary_data$n_risk) + 1 - cumsum(c(0, head(summary_data$n_events, -1)))  # adjust to start with total and decrement
  summary_data$survival_prob = 1 - summary_data$n_events / summary_data$n_risk
  summary_data$cum_survival = cumprod(summary_data$survival_prob)
  plot(summary_data$time, summary_data$cum_survival, type = "s", xlab = "Time", ylab = "Survival Probability", main = "Survival Curve")
}

#' Question 3: R function to reverse the scaling and centering transformation applied to a vector
#'
#' @param x A scaled numeric vector.
#' @return The unscaled original vector.
#' @export
#' @examples
#' original_data = c(10, 20, 30, 40, 50)
#' 
#' print(paste("original data is: ", toString(original_data)))
#' 
#' scaled_vector = scale(original_data)
#' print("Scaled Data is: ")
#' print(scaled_vector)
#' 
#' print("Unscaling the scaled data:")
#' print(unscale(scaled_vector))
unscale= function(x) {
  orig_mean = attr(x, "scaled:center")
  orig_sd = attr(x, "scaled:scale")
  if (is.null(orig_mean)) orig_mean = 0
  if (is.null(orig_sd)) orig_sd = 1
  result=(x * orig_sd) + orig_mean
  attr(result, "scaled:center") = NULL
  attr(result, "scaled:scale") = NULL
  return(result)
}

#' Question 4: Approximate Data Using PCA
#'
#' This function approximates the original data matrix `x` using the first `npc`
#' principal components. It scales and centers the data before PCA and
#' reconstructs it back to the original scale and center.
#'
#' @param x A numeric matrix or data frame of data to approximate.
#' @param npc Number of principal components to use for approximation.
#' @return A matrix of the approximated data, rescaled and recentered.
#' @examples
#' data(iris)
#' approx_data = pcApprox(iris[, 1:4], 2)
#' print(approx_data)
#' @importFrom stats aggregate dbinom prcomp sd
#' @importFrom utils head
#' @export
pcApprox = function(x, npc) {
  if (is.data.frame(x)) {
    x = data.matrix(x)
  }
  npc = min(npc, ncol(x))
  center = apply(x, 2, mean)
  scale = apply(x, 2, sd)
  x_scaled =sweep(x, 2, center, "-")
  x_scaled = sweep(x_scaled, 2, scale, "/")
  pca = prcomp(x_scaled, rank. = npc, scale. = FALSE)
  x_approx_scaled = pca$x[, 1:npc] %*% t(pca$rotation[, 1:npc])
  x_approx = sweep(x_approx_scaled, 2, scale, "*")
  x_approx = sweep(x_approx, 2, center, "+")
  return(x_approx)
}

#' Question 5: R function to standardize variable names
#'
#' Standardize Column Names
#'
#' This function standardizes the column names of a data frame to small camel case using
#' `janitor::make_clean_names` for initial cleaning and `snakecase::to_any_case`
#' for converting to the specified case format.
#'
#' @param data A data frame whose column names need to be standardized.
#' @return A data frame with standardized column names in small camel case.
#' @examples
#' sample_data = data.frame(`FiRst nAMe` = c("Swati", "Taylor"),`Last Name` = c("Rajwal", "Swift"),`Age in yeARS` = c(21, 32))
#' print(standardizeNames(sample_data))
#' @export
standardizeNames = function(data) {
  data = dplyr::rename_with(data, ~ snakecase::to_any_case(janitor::make_clean_names(tolower(names(data))), case = "small_camel"))
  return(data)
}


#' Question 6: R function to calculate min. sample size for T-Test
#'
#' This function estimates the minimum sample size required for a t-test with
#' 80% power and a significance level of 0.05. It can perform calculations
#' for both one-sample and two-sample t-tests based on the input provided.
#'
#' @param x1 Numeric vector of preliminary data for the first sample or the only sample.
#' @param x2 Optional numeric vector of preliminary data for the second sample.
#'
#' @return An integer representing the minimum sample size needed to achieve the
#' specified power for a t-test.
#'
#' @export
#'
#' @examples
#' sample_size_one = minimumN(rnorm(30, mean = 5, sd = 1.5))
#' print(paste("Minimum sample size for one-sample t-test:", sample_size_one))
#' sample_size_two = minimumN(rnorm(30, mean = 5, sd = 1.5), rnorm(30, mean = 5.5, sd = 1.5))
#' print(paste("Minimum sample size for two-sample t-test:", sample_size_two))
minimumN = function(x1, x2 = NULL) {
  alpha = 0.05   # Significance level
  power = 0.8    # Desired power
  if (is.null(x2)) {
    m = mean(x1)
    sd = sd(x1)
    d = m / sd
    result = pwr::pwr.t.test(d = d, sig.level = alpha, power = power, type = "one.sample")
  } else {
    m1 = mean(x1)
    m2 = mean(x2)
    sd1 = sd(x1)
    sd2 = sd(x2)
    n1 = length(x1)
    n2 = length(x2)
    pooled_sd = sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
    d = (m1 - m2) / pooled_sd
    result = pwr::pwr.t.test(d = d, sig.level = alpha, power = power, type = "two.sample")
  }
  return(ceiling(result$n))
}

#' Question 7: R function to download report from REDCap
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
#'
#' # downloadRedcapReport("REDCAP_API_TOKEN", "https://redcap.example.com/api/", "12345")
#'
#' # Note: Before using the function, please make sure to follow following steps:
#' #1. Close R sessions
#' #2. Create a .Renviron file in your home directory (i.e., inside 'bmi510final' folder)
#' #3. Inside the .Renviron file, define the following environment variable:
#' #REDCAP_API_TOKEN=<your_api_token_here>
#' #4. Start your R session and now the R session should be able to load the new environment variables.
#' #5. Run the following code  
downloadRedcapReport = function(redcapTokenName, redcapUrl, redcapReportId) {
  if (!requireNamespace("httr", quietly = TRUE)) {
    stop("Package 'httr' is required but not installed.")
  }
  if (!requireNamespace("readr", quietly = TRUE)) {
    stop("Package 'readr' is required but not installed.")
  }
  token = Sys.getenv(redcapTokenName)  
  if (token == "") {
    stop(paste("API token for", redcapTokenName, "not found in .Renviron file."))
  }
  formData = list(
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
  response = httr::POST(redcapUrl, body = formData, encode = "form")
  result = httr::content(response)
  return(tibble::tibble(result))
}