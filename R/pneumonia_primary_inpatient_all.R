#' Pneumonia primary inpatient all
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/pneumonia_primary_inpatient.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "480","480.%","481","481.%","482","482.%","483","483.%","484","484.%","485","485.%","486","486.%","487.0","488.11","488.81"
#'
#' ICD10:"J09.X1", "J09.X2", "J10.00", "J10.01", "J10.08", "J10.1", "J11.0", "J11.08", "J11.1", "J12.0", "J12.1", "J12.2", "J12.3", 
#'        "J12.81", "J12.89", "J12.9", "J13", "J13.%", "J14", "J14.%", "J15.3", "J15.4", "J15.7", "J15.9", "J16.0", "J16.8", "J18.0", "J18.1", "J18.8", "J18.9"
#' @export
pneumonia_primary_inpatient_all <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("480","480.%","481","481.%","482","482.%","483","483.%","484","484.%","485","485.%","486","486.%","487.0","488.11","488.81")
  icd10_codes <- c("J09.X1", "J09.X2", "J10.00", "J10.01", "J10.08", "J10.1", "J11.0", "J11.08", "J11.1", "J12.0", "J12.1", "J12.2", "J12.3", 
                   "J12.81", "J12.89", "J12.9", "J13", "J13.%", "J14", "J14.%", "J15.3", "J15.4", "J15.7", "J15.9", "J16.0", "J16.8", "J18.0", "J18.1", "J18.8", "J18.9")
  icd_codes <- c(icd9_codes,icd10_codes)
  result_all <- aou.reader::hospitalization_query(icd_codes,anchor_date_table,before,after)
  result_all <- result_all[order(hospitalization_entry_date)]
  result_all <- result_all[,.(pneumonia_primary_inpatient_status = TRUE,
                              pneumonia_primary_inpatient_entry_date = hospitalization_entry_date[1]),
                           .(person_id, hospitalization_entry_date)]
  .write_to_bucket(result_all,output_folder,"pneumonia_primary_inpatient_all")
}
