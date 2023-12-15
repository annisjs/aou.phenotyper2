#' Afib
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/afib.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "427.3","427.31","427.32"
#'
#' ICD10: "I48","I48.0","I48.1","I48.2","I48.3","I48.4","I48.9","I48.91","I48.92"
#' @export
afib <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("427.3","427.31","427.32")
    icd10_codes <- c("I48","I48.0","I48.1","I48.2","I48.3","I48.4","I48.9","I48.91","I48.92")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(dataset,icd10_codes)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(afib_entry_date = min(condition_start_date),
                                fib_status = length(condition_start_date) > 0),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"afib")
}
