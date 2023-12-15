#' Interstitial lung disease
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/interstitial_lung_disease.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "515.%","516.%","135","501","508.1","518.89"
#' ICD10: "J84.%","D86.0","J61","J70.1","J98.4"
#' @export
interstitial_lung_disease <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("515.%","516.%","135","501","508.1","518.89")
    icd10_codes <- c("J84.%","D86.0","J61","J70.1","J98.4")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(interstitial_lung_disease_status = length(condition_start_date) > 0,
                                interstitial_lung_disease_entry_date = min(condition_start_date)),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"interstitial_lung_disease")
}
