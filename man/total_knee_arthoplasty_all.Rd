% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/total_knee_arthoplasty_all.R
\name{total_knee_arthoplasty_all}
\alias{total_knee_arthoplasty_all}
\title{Total knee arthoplasty all}
\usage{
total_knee_arthoplasty_all(
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
output_folder/total_knee_arthoplasty.csv
}
\description{
Total knee arthoplasty all
}
\details{
At least 1 CPT code: 27445, 27447, 27486, 27487
}
