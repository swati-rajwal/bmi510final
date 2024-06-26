% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bmi510.r
\name{unscale}
\alias{unscale}
\title{Question 3: R function to reverse the scaling and centering transformation applied to a vector}
\usage{
unscale(x)
}
\arguments{
\item{x}{A scaled numeric vector.}
}
\value{
The unscaled original vector.
}
\description{
This function reverses the centering and scaling transformation applied to a numeric vector.
It retrieves the original mean and standard deviation attributes from the scaled vector and
uses them to rescale and recenter the data back to its original form.
}
\examples{
original_data = c(10, 20, 30, 40, 50)

print(paste("original data is: ", toString(original_data)))

scaled_vector = scale(original_data)
print("Scaled Data is: ")
print(scaled_vector)

print("Unscaling the scaled data:")
print(unscale(scaled_vector))
}
