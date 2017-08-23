# staphopia-r
A R library to extract data from the [Staphopia](https://staphopia.emory.edu) API

## Installation

Tested on R version 3.4

First install the Bioconductor Biostrings and devtools package (if not already installed)

  ```
  source("https://bioconductor.org/biocLite.R")
  biocLite("Biostrings")
  install.packages("devtools")
  library(devtools)
  ```
  
Install and test staphopia-r from github (running the test might take 1-2 min)
 
  ```
  install_github("staphopia/staphopia-r/staphopia")
  library("staphopia")
  testthat::test_package("staphopia")
  ```
  
 ## Create user_name and get authenication token
  
 See https://staphopia.emory.edu/docs/api/#authentication

### On Mac/ Linux

Create a file in your home directory called ".staphopia" for logging into the API.  The file should contain two lines.

```
TOKEN = "xxxxxxxxxxxxxxxxxxxxxx"
USE_DEV = FALSE
```

'TOKEN' is the API authenication token (USE_DEV is a variable for script development)

### On Windows
