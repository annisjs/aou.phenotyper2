#' Major depressive disorder
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/major_depressive_disorder.csv
#' @details Need at least 1 ICD code:
#'
#' ICD9: "296.21","296.22","296.23","296.24","296.25","296.26","296.2","296.31","296.32",
#' "296.33","296.34","296.35","296.36","296.3","300.4","293.83","311"
#'
#' ICD10: "F32.0","F32.1","F32.2","F32.3","F32.4","F32.5","F32.9","F33","F33.1","F33.2",
#' "F33.3","F33.41","F33.42","F33.9","F34.1","F06.30","F32.9"
#' @export
major_depressive_disorder <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("296.21","296.22","296.23","296.24","296.25","296.26","296.2","296.31","296.32","296.33","296.34","296.35","296.36","296.3","300.4","293.83","311")
    icd10_codes <- c("F32.0","F32.1","F32.2","F32.3","F32.4","F32.5","F32.9","F33","F33.1","F33.2","F33.3","F33.41","F33.42","F33.9","F34.1","F06.30","F32.9")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- setDT(result_all)[,.(major_depressive_disorder_entry_date = min(condition_start_date),
                                       major_depressive_disorder_status = length(condition_start_date) > 0),
                                    .(person_id)]
    .write_to_bucket(result_all,output_folder,"major_depressive_disorder")
}
