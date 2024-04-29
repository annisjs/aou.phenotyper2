#' Statins
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing three columns: person_id, anchor_date, statins_status. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/beta_blockers.csv
#' @details Meds: "atorvastatin","lipitor","simvastatin","zocor","
#' rosuvastatin","crestor","pravastatin","pravachol","fluvastatin",
#' "lescol","lovastatin","mevacor","vytorin","caduet"
#' @export
statins <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  meds <- c("atorvastatin","lipitor","simvastatin","zocor","rosuvastatin","crestor","pravastatin",
            "pravachol","fluvastatin","lescol","lovastatin","mevacor","vytorin","caduet")
  result <- aou.reader::med_query(meds,anchor_date_table,before,after)
  result <- result[,.(statins_status = length(drug_exposure_start_date) > 0,
                      statins_entry_date = min(drug_exposure_start_date)),.(person_id)]
  .write_to_bucket(result,output_folder,"statins")
}
