# staphopia-r
A R library to extract data from the [Staphopia](https://staphopia.emory.edu) API

## Installation

Tested on R version 3.4

First install the Bioconductor Biostrings package (if not already installed)

  ```
  source("https://bioconductor.org/biocLite.R")
  biocLite("Biostrings")
  ```
 Then install devtools (if not already installed)
 
  ```
  install.packages("devtools")
  library(devtools)
  ```
  
Install and test staphopia-r from github (running the tests might take 1-2 min)
 
  ```
  install_github("staphopia/staphopia-r/staphopia")
  library("staphopia")
  testthat::test_package("staphopia")
  ```
  
  ## Create user_name and get authenication key
  
 See https://staphopia.emory.edu/docs/api/#authentication
