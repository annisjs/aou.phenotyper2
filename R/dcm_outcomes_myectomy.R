#' dcm_outcomes_myectomy
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_myectomy.csv
#' @details At least 1 CPT code
#'
#' @export
dcm_outcomes_myectomy <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  cpt_codes <- c('33416','93583')
  result_all <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
  result_all <- result_all[,c("person_id","entry_date")]
  result_all <- result_all[order(entry_date)]

  result_all <- result_all[,.(dcm_outcomes_myectomy_status = length(entry_date) > 0,
                              dcm_outcomes_myectomy_entry_date = min(entry_date)),
                            .(person_id)]
  .write_to_bucket(result_all,output_folder,"dcm_outcomes_myectomy")
}