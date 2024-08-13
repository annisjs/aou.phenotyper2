#' Postpartum depression
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/postpartum_depression.csv
#' @details At least 1 ICD code
#'
#' ICD9: 300.4, 309.0, 311, 295-298
#'
#' ICD10:  F53.0, O90.6, F53, F53.1
#' @export
postpartum_depression <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("300.4", "309.0", "311", "311.%", "295", "295.%", "296", "296.%", "297", "297.%", "298", "298.%")
    icd10_codes <- c("F53.0", "O90.6", "F53", "F53.1")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(postpartum_depression_entry_date = min(condition_start_date),
                                postpartum_depression_status = length(condition_start_date) > 0),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"postpartum_depression")
}
