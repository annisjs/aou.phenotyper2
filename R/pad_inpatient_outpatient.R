#' PAD Inpatient/Outpatient
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/pad_inpatient_outpatient.csv
#' @details At least 1 inpatient or 2 outpatient ICD codes:
#'
#' ICD9: "443.9","443.8"
#'
#' ICD10: "I73.8","I73.9","I70.20","I70.21","I70.22"
#' @export
pad_inpatient_outpatient <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    # Pull inpatient and outpatient codes
    icd9_codes <-  c("443.9","443.8")
    icd10_codes <- c("I73.8","I73.9","I70.20","I70.21","I70.22")
    codes <- c(icd9_codes, icd10_codes)
    pad_inpt <- aou.reader::inpatient_icd_query(codes,anchor_date_table,before,after)
    pad_outpt <- aou.reader::outpatient_icd_query(codes,anchor_date_table,before,after)
    # Sort by date
    pad_inpt_sorted <- pad_inpt[order(condition_start_date)]
    pad_outpt_sorted <- pad_outpt[order(condition_start_date)]
    # De-duplicate by date and person id
    pad_inpt_sorted <- pad_inpt_sorted[!duplicated(pad_inpt_sorted, by=c("person_id", "condition_start_date"))]
    pad_outpt_sorted <- pad_outpt_sorted[!duplicated(pad_outpt_sorted, by=c("person_id", "condition_start_date"))]
    # Get counts and dates (2nd date for outpatient since we need two codes)
    pad_inpt_count <- pad_inpt_sorted[,.(pad_inpt_count = length(unique(condition_start_date)),
                                        pad_inpt_date = condition_start_date[1]),.(person_id)]
    pad_outpt_count <- pad_outpt_sorted[,.(pad_outpt_count = length(unique(condition_start_date)),
                                        pad_outpt_date = condition_start_date[2]),.(person_id)]
    # Outer join the inpatient and outpatient datasets
    pad_counts <- merge(pad_inpt_count,pad_outpt_count,by="person_id",all.x=TRUE,all.y=TRUE)
    # Definition: If 1 inpatient or 2 outpatient
    pad_counts[,pad_inpt_status := ifelse(is.na(pad_inpt_count),FALSE,pad_inpt_count >= 1)]
    pad_counts[,pad_outpt_status := ifelse(is.na(pad_outpt_count),FALSE,pad_outpt_count >= 2)]
    pad_counts[,pad_inpatient_outpatient_status := pad_inpt_status | pad_outpt_status]
    # Get the entry date (min date)
    pad_counts[,pad_inpatient_outpatient_entry_date := pmin(pad_inpt_date,pad_outpt_date,na.rm = TRUE)]
    pad_counts[,pad_inpatient_outpatient_entry_date := lubridate::as_date(
        ifelse(pad_inpatient_outpatient_status == FALSE,NA,
            pad_inpatient_outpatient_entry_date))]
    # Save data to bucket
    pad_counts <- pad_counts[,c("person_id","pad_inpatient_outpatient_entry_date","pad_inpatient_outpatient_status")]
    pad_counts <- pad_counts[pad_inpatient_outpatient_status == TRUE]
    .write_to_bucket(pad_counts,output_folder,"pad_inpatient_outpatient")
}
