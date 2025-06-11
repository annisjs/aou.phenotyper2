#' DCM OUTCOMES afib
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_afib.csv
#' @details At least 1 ICD code:
#'
#' ICD9: '427.3','427.31'
#'
#' ICD10: 'I48.0','I48.1','I48.11','I48.19','I48.2','I48.20','I48.21','I48.4','I48.9','I48.91'
#' @export
dcm_outcomes_afib <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c('427.3','427.31','427.32')
    icd10_codes <- c('I48','I48.0','I48.1','I48.11','I48.19','I48.2','I48.20','I48.21','I48.3',
                        'I48.4','I48.9','I48.91','I48.92')
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    cpt <- c('33254','33255','33256','33257','33258','33259','33265','33266','93656','93657',
                    '92960','92961','1060F','4300F')

    cpt_dat <- aou.reader::cpt_query(cpt,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    final <- rbind(result_all,cpt_dat)
    final <- final[,.(dcm_outcomes_afib_status = length(condition_start_date) > 0,
                                dcm_outcomes_afib_entry_date = min(condition_start_date)),
                                .(person_id)]
    .write_to_bucket(final,output_folder,"dcm_outcomes_afib")
}