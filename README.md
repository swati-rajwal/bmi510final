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

# 👩‍💻Working on the Package
1. Open `Terminal` or `iTerm2`.
2. Download the repository:
	* `git clone https://github.com/swati-rajwal/bmi510final`
	* `cd bmi510final`
3. Edit
4. Update documentation
	* run R, either from terminal or within RStudio
	* `library(devtools)`
	* `library(roxygen2)`
	* `document()`
	* `install()`
5. Push up changes
	* `git status` 
	* `git add [changed file]` 
	* Do not add files that you do not want to contribute, e.g., `random_scraps.R`
	* `git commit -m "a string describing your changes"` 
	* `git push` 

# 📝Usage

```r
library(bmi510final)
help(package="bmi510final")
devtools::document()
devtools::install()
devtools::check()
```

Please make sure your system has https://cran.r-project.org/bin/windows/Rtools/rtools43/rtools.html installed on your local computer while for `check()` function