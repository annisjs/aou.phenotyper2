#' Pneumonia
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/asthma.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "480","480.%","481","481.%","482","482.%","483","483.%","484","484.%","485","485.%","486","486.%","487","487.%","488","488.%"
#'
#' ICD10: "J18.%"
#' @export
pneumonia <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("480","480.%","481","481.%","482","482.%","483","483.%","484","484.%","485","485.%","486","486.%","487","487.%","488","488.%")
  icd10_codes <- c("J18.%")
  result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
  result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
  result_all <- rbind(result_icd9,result_icd10)
  result_all <- setDT(result_all)[,.(pneumonia_status = length(condition_start_date) > 0,
                                     pneumonia_entry_date = min(condition_start_date)),
                                  .(person_id)]
  .write_to_bucket(result_all,output_folder,"pneumonia")
}
