% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/non_alcoholic_liver_disease.R
\name{non_alcoholic_liver_disease}
\alias{non_alcoholic_liver_disease}
\title{Nonalcoholic liver disease}
\usage{
non_alcoholic_liver_disease(
  output_folder,
  anchor_date_table = NULL,
  before = NULL,
  after = NULL
)
}
\arguments{
\item{output_folder}{the folder to write the output}

\item{anchor_date_table}{a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.}

\item{before}{an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.}

\item{after}{an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.}
}
\value{
output_folder/non_alcoholic_liver_disease.csv
}
\description{
Nonalcoholic liver disease
}
\details{
At least 1 ICD code

ICD9: 571.8, 571.9

ICD10: K76.0, K74.1, K76.89, K76.8, K74.2, K75.81
}
