#' Myocardial Infarction inpatient All
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mi_inpatient.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "410","410.%","411","411.%"
#'
#' ICD10: "I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%"
#' @export
mi_inpatient_all <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("410","410.%","411","411.%")
  icd10_codes <- c("I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%")
  icd_codes <- c(icd9_codes,icd10_codes)
  result_all <- aou.reader::inpatient_icd_query(icd_codes,anchor_date_table,before,after)
  result_all <- result_all[order(condition_start_date)]
  result_all <- result_all[,.(mi_inpatient_status = TRUE,
                              mi_inpatient_entry_date = condition_start_date[1]),
                           .(person_id, condition_start_date)]
  .write_to_bucket(result_all,output_folder,"mi_inpatient_all")
}
