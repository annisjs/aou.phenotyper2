#' Coronary Artery Bypass Graft All
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/coronary_artery_bypass_graft.csv
#' @details At least 1 CPT code: 
#' "33510","33511","33512","33513","33514","33516","33517","33518","33519","33521","33522","33523","33533","33534","33535","33536"
#'
#' @export
coronary_artery_bypass_graft_all <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  cpt_codes <- c("33510","33511","33512","33513","33514","33516","33517","33518","33519","33521","33522","33523","33533","33534","33535","33536")
  result_all <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
  result_all <- result_all[,c("person_id","entry_date")]
  result_all <- result_all[order(entry_date)]
  result_all <- result_all[,.(coronary_artery_bypass_graft_entry_date = entry_date[1],
                              coronary_artery_bypass_graft_status = TRUE),
                           .(person_id, entry_date)]
  .write_to_bucket(result_all,output_folder,"coronary_artery_bypass_graft_all")
}