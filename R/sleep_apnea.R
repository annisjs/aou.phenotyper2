#' Sleep Apnea
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details At least 1 ICD code
#'
#' ICD9: "780.51","780.53","780.57","327.2%"
#'
#' ICD10: "G47.3%"
#' @return output_folder/sleep_apnea.csv
#' @export
sleep_apnea <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{

  icd9_codes <- c("780.51","780.53","780.57","327.2%")
  icd10_codes <- c("G47.3%")
  result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
  result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
  result_all <- rbind(result_icd9,result_icd10)
  result_all <- setDT(result_all)[,.(sleep_apnea_entry_date = min(condition_start_date),
                                     sleep_apnea_status = length(condition_start_date) > 0),
                                  .(person_id)]
  .write_to_bucket(result_all,output_folder,"sleep_apnea")
}
