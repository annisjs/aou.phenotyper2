#' Insomnia
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/insomnia.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "307.41","327.01","327.02","327.09","780.52","307.42","327.0","327.00"
#'
#' ICD10: "F51.04","G47.00","G47.0","G47.09","F51.05","G47.01","F51.02","F51.03","F51.01","F51.09"
#' @export
insomnia <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("307.41","327.01","327.02","327.09","780.52","307.42","327.0","327.00")
    icd10_codes <- c("F51.04","G47.00","G47.0","G47.09","F51.05","G47.01","F51.02","F51.03","F51.01","F51.09")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(insomnia_entry_date = min(condition_start_date),
                                insomnia_status = length(condition_start_date) > 0),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"insomnia")
}
