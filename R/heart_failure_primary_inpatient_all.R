#' Heart failure primary inpatient all
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/heart_failure_primary_inpatient.csv
#' @details At least 1 inpatient ICD code:
#'
#' ICD9: "425","425.%","428","428.%",
#'
#' ICD10: "I42","I42.%","I50","I50.%"
#' @export
heart_failure_primary_inpatient_all <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  codes <- c("425","425.%","428","428.%","I42","I42.%","I50","I50.%")
  result_all <- aou.reader::hospitalization_query(codes,anchor_date_table,before,after)
  result_all <- result_all[order(hospitalization_entry_date)]
  result_all <- result_all[,.(heart_failure_primary_inpatient_entry_date = hospitalization_entry_date[1],
                              heart_failure_primary_inpatient_status = TRUE),
                           .(person_id, hospitalization_entry_date)]
  .write_to_bucket(result_all,output_folder,"heart_failure_primary_inpatient_all")
}
