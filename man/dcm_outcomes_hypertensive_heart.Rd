% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcm_outcomes_hypertensive_heart.R
\name{dcm_outcomes_hypertensive_heart}
\alias{dcm_outcomes_hypertensive_heart}
\title{DCM OUTCOMES hypertensive_heart}
\usage{
dcm_outcomes_hypertensive_heart(
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
output_folder/dcm_outcomes_hypertensive_heart.csv
}
\description{
DCM OUTCOMES hypertensive_heart
}
\details{
At least 1 ICD code:
}
