#' CABG
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/cabg.csv
#' @details At least 1 CPT code:
#'
#' 33510, 33511, 33512, 33513, 33514, 33516, 33517, 33518, 33519, 33521, 33522, 33523,  33533, 33534, 33535, 33536
#'
#' @export
cabg <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    cpt_codes <- c("33510", "33511", "33512", "33513", "33514", "33516", "33517", 
                   "33518", "33519", "33521", "33522", "33523", "33533", "33534", 
                   "33535", "33536")
    result <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
    result <- result[order(entry_date)]
    result_all <- result_all[,.(cabg_status = TRUE,
                                cabg_entry_date = entry_date[1]),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"cabg")
}
