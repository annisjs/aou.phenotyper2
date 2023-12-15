# Overview

aou.phenotyper is an R package whose goal is to assist in data pulls and analysis in the All of Us Researcher workbench.

# Installation
In an R notebook on the AOU workbench:
```r
devtools::install_github("annisjs/aou.phenotyper2",upgrade=F)
devtools::install_github("annisjs/aou.bucket",upgrade=F) # Installed to use "read_bucket" function
```

# Example
```r
library(stringr)
library(aou.bucket)
library(aou.phenotyper2)

# The location of the output in the workspace bucket
output_folder <- "datasets"

afib(output_folder)
smoking(output_folder)
education(output_folder)
demographics(output_folder)

afib_dat <- read_bucket("datasets/afib.csv")
smoking_dat <- read_bucket("datasets/smoking.csv")
education_dat <- read_bucket("datasets/education.csv") 
dem <- read_bucket("datasets/demographics.csv")
```
