library(bmi510final)

# Question 1
print(logLikBernoulli(c(1, 0, 0, 0, 1, 1, 1)))

# Question 2
survival_data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
survCurv(survival_data$status, survival_data$time)

# Question 3
scaled_vector = scale(c(10, 20, 30, 40, 50))
print(scaled_vector)
print(unscale(scaled_vector))

# Question 4
data(iris)
approx_data = pcApprox(iris[, 1:4], 2)
print(head(approx_data))

# Question 5
sample_data = data.frame(
`FiRst nAMe` = c("Swati", "Taylor"),
`Last Name` = c("Rajwal", "Swift"),
`Age in yeARS` = c(21, 32))
print(standardizeNames(sample_data))

# Question 6
sample_size_one = minimumN(rnorm(30, mean = 5, sd = 1.5))
print(paste("Minimum sample size for one-sample t-test:", sample_size_one))
sample_size_two = minimumN(rnorm(30, mean = 5, sd = 1.5), rnorm(30, mean = 5.5, sd = 1.5))
print(paste("Minimum sample size for two-sample t-test:", sample_size_two))

# Question 7
downloadRedcapReport("REDCAP_API_TOKEN", "https://redcap.emory.edu/api/", "46524")
