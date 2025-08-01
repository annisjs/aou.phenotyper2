% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/mi_primary_inpatient_all.R
\name{mi_primary_inpatient_all}
\alias{mi_primary_inpatient_all}
\title{Myocardial Infarction primary inpatient all}
\usage{
mi_primary_inpatient_all(
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
output_folder/mi_primary_inpatient.csv
}
\description{
Myocardial Infarction primary inpatient all
}
\details{
At least 1 ICD code:

ICD9: "410","410.\%","411","411.\%"

ICD10: "I21","I21.\%","I22","I22.\%","I23","I23.\%","I24","I24.\%","I25","I25.\%"
}
