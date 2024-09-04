#' All DCM codes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_dcm_codes.csv
#' ICD9: "425.5","674.50","674.51","674.52","674.53","674.54"
#'
#' ICD10: "I42.0","A36.81","B33.24","I42.6","I42.7","O90.3"
#' @export
all_dcm_codes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("425.5","674.50","674.51","674.52","674.53","674.54")
    icd10_codes <- c("I42.0","A36.81","B33.24","I42.6","I42.7","O90.3")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    colnames(result_all) <- c("person_id","all_dcm_codes_entry_date","all_dcm_codes_value")
    .write_to_bucket(result_all,output_folder,"all_dcm_codes")
}
