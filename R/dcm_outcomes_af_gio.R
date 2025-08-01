#' DCM OUTCOMES af_gio
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_af_gio.csv
#' @details At least 1 ICD code:
#' @export
dcm_outcomes_af_gio <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c('427.3','427.31')
    icd10_codes <- c('I48.0','I48.1','I48.11','I48.19','I48.2','I48.20','I48.21','I48.4','I48.9','I48.91')
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(dcm_outcomes_af_gio_status = length(condition_start_date) > 0,
                                dcm_outcomes_af_gio_entry_date = min(condition_start_date)),
                                .(person_id)]
    .write_to_bucket(result_all,output_folder,"dcm_outcomes_af_gio")
}