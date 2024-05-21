#' COPD primary inpatient
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/copd_primary_inpatient.csv
#' @details Definition: Needs at least 1 inpatient ICD code:
#'
#' ICD9: "491","491.0","491.1","491.2","491.20","491.21","491.22",
#' "491.8","491.9","492","492.0","492.8","496","496.0","493.21","493.22"
#'
#' ICD10: "J44.%","J43.%","J42","J41.%"
#'
#' @export
copd_primary_inpatient <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("491","491.0","491.1","491.2","491.20","491.21","491.22","491.8","491.9","492","492.0","492.8","496","496.0","493.21","493.22")
    icd10_codes <- c("J44.%","J43.%","J42","J41.%")
    icd_codes <- c(icd9_codes,icd10_codes)
    result_all <- aou.reader::hospitalization_query(icd_codes,anchor_date_table,before,after)
    result_all <- result_all[order(hospitalization_entry_date)]
    result_all <- result_all[,.(copd_primary_inpatient_status = TRUE,
                                copd_primary_inpatient_entry_date = hospitalization_entry_date[1]),
                              .(person_id)]
    .write_to_bucket(result_all,output_folder,"copd_primary_inpatient")
}
