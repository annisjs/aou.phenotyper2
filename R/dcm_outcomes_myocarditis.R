#' DCM OUTCOMES myocarditis
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_myocarditis.csv
#' @details At least 1 ICD code:
#' @export
dcm_outcomes_myocarditis <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c('074.23','422','422.9','422.91','422.99','422.9','422','429')
    icd10_codes <- c('B33.22','I40.1','I40.8','I40.9','I41','I51.4','J10.82','J11.82')
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(dcm_outcomes_myocarditis_status = length(condition_start_date) > 0,
                                dcm_outcomes_myocarditis_entry_date = min(condition_start_date)),
                                .(person_id)]
    .write_to_bucket(result_all,output_folder,"dcm_outcomes_myocarditis")
}