% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/wear_time.R
\name{wear_time}
\alias{wear_time}
\title{Fitbit wear time}
\usage{
wear_time(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
}
\arguments{
\item{output_folder}{the folder to write the output}

\item{anchor_date_table}{a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.}

\item{before}{an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.}

\item{after}{an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.}
}
\value{
output_folder/wear_time.csv
}
\description{
Fitbit wear time
}
\details{
An hour of wear time is defined when step count is > 0 for a given hour of the day,
}
