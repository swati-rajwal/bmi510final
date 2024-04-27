library(bmi510final)
survival_data = read.csv("https://jlucasmckay.bmi.emory.edu/global/bmi510/Labs-Materials/survival.csv")
# Test the survCurv function with the loaded data

survCurv(survival_data$status, survival_data$time)
