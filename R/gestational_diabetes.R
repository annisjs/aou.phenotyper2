#' Gestational diabetes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/gestational_diabetes.csv
#' @details At least 1 ICD code
#'
#' ICD9: 648.00-648.04, 648.80-648.84
#'
#' ICD10:  O24.410, O24.414, O24.419, O24.420, O24.424, O24.429, O24.430, O24.434, 24.439, O24.4. O24.41, O24.42, O24.43, O24.415, O24.435
#' @export
gestational_diabetes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("648.00","648.01","648.02","648.03","648.04","648.80","648.81","648.82","648.83","648.84")
    icd10_codes <- c("O24.410", "O24.414", "O24.419", "O24.420", "O24.424", "O24.429", "O24.430", "O24.434", "24.439", "O24.4", "O24.41", "O24.42", "O24.43", "O24.415", "O24.435")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(gestational_diabetes_entry_date = min(condition_start_date),
                                gestational_diabetes_status = length(condition_start_date) > 0),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"gestational_diabetes")
}
