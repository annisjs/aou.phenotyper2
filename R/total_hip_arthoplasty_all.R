#' Total hip arthoplasty all
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/total_hip_arthoplasty.csv
#' @details At least 1 CPT code: 27130, 27132, 27134, 27137, 27138
#'
#' @export
total_hip_arthoplasty_all <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  cpt_codes <- c("27130", "27132", "27134", "27137", "27138")
  result_all <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
  result_all <- result_all[,c("person_id","entry_date")]
  result_all <- result_all[order(entry_date)]
  result_all <- result_all[,.(total_hip_arthoplasty_entry_date = entry_date[1],
                              total_hip_arthoplasty_status = TRUE),
                           .(person_id, entry_date)]
  .write_to_bucket(result_all,output_folder,"total_hip_arthoplasty_all")
}