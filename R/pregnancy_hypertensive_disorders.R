#' Pregnancy hypertensive disorders
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/pregnancy_hypertensive_disorders.csv
#' @details At least 1 ICD code
#'
#' ICD9: 64240, 64241, 64242, 64243, 64244, 64250, 64251, 64252, 64253, 64254, 64260, 64261, 64262, 64263, 64264, 64270, 64271, 64272, 64273, 64274
#'
#' ICD10:  O13, O14, O14.00, O14.02, O14.03, O14.04, O14.05, O14.10, O14.12, O14.13, O14.90, O14.92, O14.93, O14.14, O14.15, O14.94, O14.95, O14.1, O15.00, O15.02, O15.03, O15.1, O15.9, O14.20, O14.22, O14.23, O14.24, O14.25, O11, O11.1, O11.2, O11.3, O11.4, O11.5, O11.9
#' @export
pregnancy_hypertensive_disorders <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("642.40", "642.41", "642.42", "642.43", "642.44", "642.50", "642.51", "642.52", "642.53", "642.54", "642.60", "642.61", "642.62", "642.63", "642.64", "642.70", "642.71", "642.72", "642.73", "642.74")
    icd10_codes <- c("O13","013.%","O14", "O14.00", "O14.02", "O14.03", "O14.04", "O14.05", "O14.10", "O14.12", "O14.13", "O14.90", "O14.92", "O14.93", "O14.14", "O14.15", "O14.94", "O14.95", "O14.1", "O15.00", "O15.02", "O15.03", "O15.1", "O15.9", "O14.20", "O14.22", "O14.23", "O14.24", "O14.25", "O11", "O11.1", "O11.2", "O11.3", "O11.4", "O11.5", "O11.9")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(pregnancy_hypertensive_disorders_entry_date = min(condition_start_date),
                                pregnancy_hypertensive_disorders_status = length(condition_start_date) > 0),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"pregnancy_hypertensive_disorders")
}
