#' Stroke
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/stroke.csv
#' @details At lest 1 ICD code:
#'
#' ICD9: "433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91","436.%"
#'
#' ICD10: "I63.%","G46.3","G46.4"
#' @export
stroke <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91","436.%")
    icd10_codes <- c("I63.%","G46.3","G46.4")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- setDT(result_all)[,.(stroke_entry_date = min(condition_start_date),
                                        stroke_status = length(condition_start_date) > 0),
                                    .(person_id)]
    .write_to_bucket(result_all,output_folder,"stroke")
}
