% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/newborn_delivery.R
\name{newborn_delivery}
\alias{newborn_delivery}
\title{Delivery of newborn}
\usage{
newborn_delivery(
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
output_folder/newborn_delivery.csv
}
\description{
Delivery of newborn
}
\details{
At least 1 CPT code

CPT codes for vaginal delivery: "59400", "59409", "59410", "59610", "59612", "59614"
CPT codes for cesarean delivery: "59510", "59514", "59515", "59618", "59620", "59622"
}
