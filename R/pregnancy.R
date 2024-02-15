#' Pregnancy
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/pregnancy.csv
#' @details At least 1 ICD code
#'
#' ICD9: "V22.%","V23.%"
#'
#' ICD10: "Z33.%"
#' @export
pregnancy <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("V22","V22.%","V23","V23.%")
    icd10_codes <- c("Z33","Z33.%")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(pregnancy_status = length(condition_start_date) > 0,
                                pregnancy_entry_date = min(condition_start_date)),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"pregnancy")
}
