#' Acute Heart Failure
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/acute_heart_failure.csv
#' @details At least 1 ICD code:
#'
#' ICD9: 428.21, 429.23, 428.31, 428.33, 428.41, 428.43
#'
#' ICD10: I50.21, I50.23, I50.31, I50.33, I50.41, I50.43
#' @export
acute_heart_failure <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("428.21", "429.23", "428.31", "428.33", "428.41", "428.43")
    icd10_codes <- c("I50.21", "I50.23", "I50.31", "I50.33", "I50.41", "I50.43")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[order(condition_start_date)]
    result_all <- result_all[,.(acute_heart_failure_entry_date = condition_start_date[1],
                                acute_heart_failure_status = TRUE),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"acute_heart_failure")
}
