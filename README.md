# Overview

AOUphenotyper is an R package whose goal is to assist in data pulls and analysis in the All of Us Researcher workbench.

# Installation
In an R notebook on the AOU workbench:
```r
devtools::install_github("annisjs/AOUphenotyper",upgrade=F)
```

# Example
```r
library(stringr)
library(AOUphenotyper)

# Your workspace bucket
my_bucket <- Sys.getenv("WORKSPACE_BUCKET")

# The AOU dataset name
dataset <- Sys.getenv("WORKSPACE_CDR")

# The location of the output in the workspace bucket
output_folder <- str_glue("{my_bucket}/datasets") 

afiboutput_folder)
smokingoutput_folder)
educationoutput_folder)
demographicsoutput_folder)

afib_dat <- read_bucket("datasets/afib.csv")
smoking_dat <- read_bucket("datasets/smoking.csv")
education_dat <- read_bucket("datasets/education.csv") 
dem <- read_bucket("datasets/demographics.csv")
```
