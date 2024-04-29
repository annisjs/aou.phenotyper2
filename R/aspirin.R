#' Aspirin
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing three columns: person_id, anchor_date, aspirin_status. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/aspirin.csv
#' @details Meds: "aspirin" and "asprin" for alternative spelling/typos
#' @export
aspirin <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("aspirin", "asprin")
  result <- aou.reader::med_query(meds,anchor_date_table,before,after)
  result <- result[,.(aspirin_status = length(drug_exposure_start_date) > 0,
                      aspirin_entry_date = min(drug_exposure_start_date)),.(person_id)]
  .write_to_bucket(result,output_folder,"aspirin")
}
