#' Obesity
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/obesity.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "278.0%","278.1%","V85.3%","V85.4%"
#'
#' ICD10: "E66","E66.%"
#' @import data.table stringr aou.reader
#' @export
obesity <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("278.0%","278.1%","V85.3%","V85.4%")
    icd10_codes <- c("E66","E66.%")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- setDT(result_all)[,.(obesity_entry_date = min(condition_start_date),
                                        obesity_status = length(condition_start_date) > 0),
                                    .(person_id)]
    .write_to_bucket(result_all,output_folder,"obesity")
}
