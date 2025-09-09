#' MI Inpatient/Outpatient
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mi_inpatient_outpatient.csv
#' @details At least 1 inpatient or 2 outpatient ICD codes:
#'
#' ICD9: "410","410.%","411","411.%"
#'
#' ICD10: "I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%"
#' @export
mi_inpatient_outpatient <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    # Pull inpatient and outpatient codes
    icd9_codes <- c("410","410.%","411","411.%")
    icd10_codes <- c("I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%")
    codes <- c(icd9_codes, icd10_codes)
    mi_inpt <- aou.reader::inpatient_icd_query(codes,anchor_date_table,before,after)
    mi_outpt <- aou.reader::outpatient_icd_query(codes,anchor_date_table,before,after)
    # Sort by date
    mi_inpt_sorted <- mi_inpt[order(condition_start_date)]
    mi_outpt_sorted <- mi_outpt[order(condition_start_date)]
    # De-duplicate by date and person id
    mi_inpt_sorted <- mi_inpt_sorted[!duplicated(mi_inpt_sorted, by=c("person_id", "condition_start_date"))]
    mi_outpt_sorted <- mi_outpt_sorted[!duplicated(mi_outpt_sorted, by=c("person_id", "condition_start_date"))]
    # Get counts and dates (2nd date for outpatient since we need two codes)
    mi_inpt_count <- mi_inpt_sorted[,.(mi_inpt_count = length(unique(condition_start_date)),
                                        mi_inpt_date = condition_start_date[1]),.(person_id)]
    mi_outpt_count <- mi_outpt_sorted[,.(mi_outpt_count = length(unique(condition_start_date)),
                                        mi_outpt_date = condition_start_date[2]),.(person_id)]
    # Outer join the inpatient and outpatient datasets
    mi_counts <- merge(mi_inpt_count,mi_outpt_count,by="person_id",all.x=TRUE,all.y=TRUE)
    # Definition: If 1 inpatient or 2 outpatient
    mi_counts[,mi_inpt_status := ifelse(is.na(mi_inpt_count),FALSE,mi_inpt_count >= 1)]
    mi_counts[,mi_outpt_status := ifelse(is.na(mi_outpt_count),FALSE,mi_outpt_count >= 2)]
    mi_counts[,mi_inpatient_outpatient_status := mi_inpt_status | mi_outpt_status]
    # Get the entry date (min date)
    mi_counts[,mi_inpatient_outpatient_entry_date := pmin(mi_inpt_date,mi_outpt_date,na.rm = TRUE)]
    mi_counts[,mi_inpatient_outpatient_entry_date := lubridate::as_date(
        ifelse(mi_inpatient_outpatient_status == FALSE,NA,
            mi_inpatient_outpatient_entry_date))]
    # Save data to bucket
    mi_counts <- mi_counts[,c("person_id","mi_inpatient_outpatient_entry_date","mi_inpatient_outpatient_status")]
    mi_counts <- mi_counts[mi_inpatient_outpatient_status == TRUE]
    .write_to_bucket(mi_counts,output_folder,"mi_inpatient_outpatient")
}
