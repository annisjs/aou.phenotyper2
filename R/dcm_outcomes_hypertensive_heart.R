#' DCM OUTCOMES hypertensive_heart
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_hypertensive_heart.csv
#' @details At least 1 ICD code:
#' @export
dcm_outcomes_hypertensive_heart <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c('402.00','402.01','402.10','402.11','402.90','402.91','404.00','404.01','404.02',
                        '404.03','404.10','404.11','404.12','404.13','404.90','404.91','404.92','404.93')
    icd10_codes <- c('I11','I11.0','I11.9','I13','I13.0','I13.1','I13.10','I13.11','I13.2')
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(dcm_outcomes_hypertensive_heart_status = length(condition_start_date) > 0,
                                dcm_outcomes_hypertensive_heart_entry_date = min(condition_start_date)),
                                .(person_id)]
    .write_to_bucket(result_all,output_folder,"dcm_outcomes_hypertensive_heart")
}