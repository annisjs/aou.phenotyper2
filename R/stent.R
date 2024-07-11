#' Stent
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/stent.csv
#' @details At least 1 CPT code: 
#' "92920", "92921", "92924", "92925", "92928", "92929", "92933", "92934", "92937", "92938", "92941", "92943", "92944", "92975", "92977", "92973", "92974", "92978", "92979"
#'
#' @export
stent <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  cpt_codes <- c("92920", "92921", "92924", "92925", "92928", "92929", "92933", "92934", "92937", "92938", "92941", "92943", "92944", "92975", "92977", "92973", "92974", "92978", "92979")
  result_all <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
  result_all <- result_all[,c("person_id","entry_date")]
  result_all <- result_all[order(entry_date)]
  result_all <- result_all[,.(stent_entry_date = entry_date[1],
                              stent_status = TRUE),
                           .(person_id)]
  .write_to_bucket(result_all,output_folder,"stent")
}