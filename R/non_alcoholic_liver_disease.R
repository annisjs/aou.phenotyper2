#' Nonalcoholic liver disease
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/non_alcoholic_liver_disease.csv
#' @details At least 1 ICD code
#'
#' ICD9: 571.8, 571.9
#'
#' ICD10: K76.0, K74.1, K76.89, K76.8, K74.2, K75.81
#' @export
non_alcoholic_liver_disease <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("571.8", "571.9")
    icd10_codes <- c("K76.0", "K74.1", "K76.89", "K76.8", "K74.2", "K75.81")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[order(condition_start_date)]
    result_all <- result_all[,.(non_alcoholic_liver_disease_entry_date = condition_start_date[1],
                                non_alcoholic_liver_disease_status = TRUE),
                .(person_id)]
    .write_to_bucket(result_all,output_folder,"non_alcoholic_liver_disease")
}