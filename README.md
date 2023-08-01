[![DOI](https://zenodo.org/badge/59863793.svg)](https://zenodo.org/badge/latestdoi/59863793)

# As of August 31st, 2023 the Staphopia API has reached end-of-life

If your research was affected by this shut down, please reach out to Tim Read and/or Robert Petit.


# staphopia-r
A R library to extract data from the Staphopia API

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
  test_staphopia()
  ```

### On Mac/ Linux

Create a file in your home directory called ".staphopia" for logging into the API.  The file should contain two lines.

```
TOKEN = "xxxxxxxxxxxxxxxxxxxxxx"
USE_DEV = FALSE
```

For 'TOKEN' substitute the API authenication token string acquired as described above (USE_DEV is a variable for script development and can be ignored)

### On Windows

TBD

[Example Commands](./staphopiaR_tutorial.Rmd)
