% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dcm_outcomes_bi_tri_fascicular_block.R
\name{dcm_outcomes_bi_tri_fascicular_block}
\alias{dcm_outcomes_bi_tri_fascicular_block}
\title{DCM OUTCOMES bi_tri_fascicular_block}
\usage{
dcm_outcomes_bi_tri_fascicular_block(
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
output_folder/dcm_outcomes_bi_tri_fascicular_block.csv
}
\description{
DCM OUTCOMES bi_tri_fascicular_block
}
\details{
At least 1 ICD code:

ICD9:'426.51','426.52'

ICD10: 'I45.2','I45.3'
}
