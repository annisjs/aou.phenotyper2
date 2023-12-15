#' Dementia
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dementia.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "331.0","331.82","290.4","290.40","290.41","290.42","290.43"
#'
#' ICD10: "G30","F00","G30.0","G30.1","G30.8","G30.9","F00.0","F00.1",
#' "F00.2","F00.9","G31.8","F02.8","F01","F01.0","F01.1","F01.2","F01.3","F01.8","F01.9"
#' @export
dementia <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("331.0","331.82","290.4","290.40","290.41","290.42","290.43")
    icd10_codes <- c("G30","F00","G30.0","G30.1","G30.8","G30.9","F00.0","F00.1","F00.2","F00.9","G31.8","F02.8","F01","F01.0","F01.1","F01.2","F01.3","F01.8","F01.9")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[,.(dementia_entry_date = min(condition_start_date),
                                dementia_status = length(condition_start_date) > 0),
                                .(person_id)]
    .write_to_bucket(result_all,output_folder,"dementia")
}
