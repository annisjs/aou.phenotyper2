#' dcm_outcomes_cied
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_cied.csv
#' @details At least 1 CPT code
#'
#' @export
dcm_outcomes_cied <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  cpt_codes <- c('33200','33202','33203','33206','33207','33208','33210','33211','33212','33213',
                     '33214','33215','33221','33222','33224','33225','33226','33227','33228','33229',
                     '33233','33234','33235','33236','33237','33238','33274','33275','71090','93279',
                     '93280','93281','93286','93288','93293','93294','93296','93731','93732','93733',
                     '93734','93735','93736')
  result_all <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
  result_all <- result_all[,c("person_id","entry_date")]
  result_all <- result_all[order(entry_date)]

  result_all <- result_all[,.(dcm_outcomes_cied_status = length(entry_date) > 0,
                              dcm_outcomes_cied_entry_date = min(entry_date)),
                            .(person_id)]
  .write_to_bucket(result_all,output_folder,"dcm_outcomes_cied")
}