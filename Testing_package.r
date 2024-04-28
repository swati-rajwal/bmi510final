library(bmi510final)

# Question 1
data <- c(1, 0, 0, 0, 1, 1, 1)
result <- logLikBernoulli(data)
print(result)

# Question 2
survival_data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
survCurv(survival_data$status, survival_data$time)

# Question 3
data_vector <- c(10, 20, 30, 40, 50)
scaled_vector <- scale(data_vector)
print(scaled_vector)
original_vector <- unscale(scaled_vector)
print(original_vector)

# Question 4
data(iris)
approx_data <- pcApprox(iris[, 1:4], 2)
print(head(approx_data))

# Question 5
sample_data <- data.frame(
`First name` = c("Swati", "Taylor"),
`LAST NAME` = c("Rajwal", "Swift"),
`Age (years)` = c(21, 32), 
check.names = FALSE)
standardized_data <- standardizeNames(sample_data)
print(standardized_data)

# Question 7
downloadRedcapReport("REDCAP_API_TOKEN", "https://redcap.emory.edu/api/", "46524")