#' Semaglutide
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return CSV saved to output_folder/semaglutide.csv
#' @details Meds: "semaglutide","ozempic","rybelsus","wegovy"
#' @export
semaglutide <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	meds <- c("semaglutide","ozempic","rybelsus","wegovy")
	result <- aou.reader::med_query(meds,anchor_date_table,before,after)
	result <- result[order(drug_exposure_start_date)]
	result <- result[,.(semaglutide_entry_date = drug_exposure_start_date[1],
						semaglutide_status = TRUE),
					  .(person_id)]
	.write_to_bucket(result,output_folder,"semaglutide")
}
