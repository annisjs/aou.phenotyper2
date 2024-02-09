#' Heart failure
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/heart_failure.csv
#' @details At least 1 inpatient or 2 outpatient ICD codes:
#'
#' ICD9: "425","425.%","428","428.%",
#'
#' ICD10: "I42","I42.%","I50","I50.%"
#' @export
heart_failure <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    # Pull inpatient and outpatient codes
    codes <- c("425","425.%","428","428.%","I42","I42.%","I50","I50.%")
    hf_inpt <- aou.reader::inpatient_icd_query(codes,anchor_date_table,before,after)
    hf_outpt <- aou.reader::outpatient_icd_query(codes,anchor_date_table,before,after)
    # Sort by date
    hf_inpt_sorted <- hf_inpt[order(condition_start_date)]
    hf_outpt_sorted <- hf_outpt[order(condition_start_date)]
    # De-duplicate
    hf_inpt_sorted <- hf_inpt_sorted[!duplicated(hf_inpt_sorted)]
    hf_outpt_sorted <- hf_outpt_sorted[!duplicated(hf_outpt_sorted)]
    # Get counts and dates (2nd date for outpatient since we need two codes)
    hf_inpt_count <- hf_inpt_sorted[,.(hf_inpt_count = length(unique(condition_start_date)),
                                        hf_inpt_date = condition_start_date[1]),.(person_id)]
    hf_outpt_count <- hf_outpt_sorted[,.(hf_outpt_count = length(unique(condition_start_date)),
                                        hf_outpt_date = condition_start_date[2]),.(person_id)]
    # Outer join the inpatient and outpatient datasets
    hf_counts <- merge(hf_inpt_count,hf_outpt_count,by="person_id",all.x=TRUE,all.y=TRUE)
    # HF definition: If 1 inpatient or 2 outpatient
    hf_counts[,hf_inpt_status := ifelse(is.na(hf_inpt_count),FALSE,hf_inpt_count >= 1)]
    hf_counts[,hf_outpt_status := ifelse(is.na(hf_outpt_count),FALSE,hf_outpt_count >= 2)]
    hf_counts[,heart_failure_status := hf_inpt_status | hf_outpt_status]
    # Get the heart failure entry date (min date)
    hf_counts[,heart_failure_entry_date := pmin(hf_inpt_date,hf_outpt_date,na.rm = TRUE)]
    hf_counts[,heart_failure_entry_date := lubridate::as_date(
        ifelse(heart_failure_status == FALSE,NA,
            heart_failure_entry_date))]
    # Save data to bucket
    hf_counts <- hf_counts[,c("person_id","heart_failure_entry_date","heart_failure_status")]
    hf_counts <- hf_counts[heart_failure_status == TRUE]
    .write_to_bucket(hf_counts,output_folder,"heart_failure")
}
