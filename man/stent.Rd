% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stent.R
\name{stent}
\alias{stent}
\title{Stent}
\usage{
stent(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
}
\arguments{
\item{output_folder}{the folder to write the output}

\item{anchor_date_table}{a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.}

\item{before}{an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.}

\item{after}{an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.}
}
\value{
output_folder/stent.csv
}
\description{
Stent
}
\details{
At least 1 CPT code:
"92920", "92921", "92924", "92925", "92928", "92929", "92933", "92934", "92937", "92938", "92941", "92943", "92944", "92975", "92977", "92973", "92974", "92978", "92979"
}
