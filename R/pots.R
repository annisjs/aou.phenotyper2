#' Postural Tachycardia Syndrome (POTS)
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/pots.csv
#' @details At least 1 ICD code:
#'
#' ICD9: 337.9
#'
#' ICD10: G90.A, G90.9
#' @export
pots <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("337.9")
    icd10_codes <- c("G90.A", "G90.9")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[order(condition_start_date)]
    result_all <- result_all[,.(pots_entry_date = condition_start_date[1],
                                pots_status = TRUE),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"pots")
}