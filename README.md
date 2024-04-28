# 🎯Quick Start: Using the Package
This tutorial describes how to access functionality in `bmi510final` R Package.

1. Open `R` in your terminal (CMD/Gitbash).
2. Download and install the package repository:
	```r 
    library(devtools)
    install_github("https://github.com/swati-rajwal/bmi510final")
    ```
    or
   
    ```r
    devtools::install_github("https://github.com/swati-rajwal/bmi510final")
    ```
# 🏃Running Examples
1. Go to `inst/` folder and it contains `Testing_package.r` file with examples.
2. Alternatively, once you install the package on your system, you can run `help(package = bmi510final)` and follow this short GIF to run sample examples for any function provided by this package:


https://github.com/swati-rajwal/bmi510final/assets/145946818/9b132ed0-b90f-487c-af51-8c1f04c63a9d


# 📌Functions Provided:
This package provides a suite of tools designed to facilitate common tasks in biomedical informatics research. It includes functions for analyzing `Bernoulli distributions`, computing `survival curves`, `unscaling standardized data`, `approximating principal components`, `standardizing variable names` in datasets, determining `minimum sample sizes for statistical tests`, and `securely fetching data from RedCap`. 
The primary goal is to create reliable, reusable code that enhances productivity and ensures consistency across data analyses. All functions are well-documented and accessible for easy integration into research workflows.

# 👩‍💻Working on the Package
1. Open `Terminal` or `iTerm2`.
2. Download the repository:
	* `git clone https://github.com/swati-rajwal/bmi510final`
	* `cd bmi510final`
3. Make changes in the code as you wish
4. Then run `r` either in cmd or gitbash or RStudio
   ```r
   library(devtools)
   library(roxygen2)
   document()
   install()
   ```
   or
   
   ```r
   library(bmi510final)
   help(package="bmi510final")
   devtools::document()
   devtools::install()
   devtools::check()
   ```
   Please make sure your system has https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html installed on your local computer while for `check()` function
  
7. Push up changes
	* `git status` 
	* `git add .`
 	* `git commit –m "added minimumN()"`
 	* `git push` 
