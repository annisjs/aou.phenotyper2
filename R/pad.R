#' Peripheral artery disease
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details At least 1 ICD code:
#'
#' ICD9: 440.2x, 443.9, 444.22
#' ICD10 : I70.2xx, I73.9, I74.x
#' 
#' @return output_folder/pad.csv
#' @export
pad <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <-  c("440.2%", "443.9", "444.22")
  icd10_codes <- c("I70.2%", "I73.9", "I74", "I74.%")
  result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
  result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
  result_all <- rbind(result_icd9,result_icd10)
  result_all <- setDT(result_all)[,.(pad_status = length(condition_start_date) > 0,
                                     pad_entry_date = min(condition_start_date)),
                                  .(person_id)]
  .write_to_bucket(result_all,output_folder,"pad")
}
