#' Stroke Inpatient/Outpatient
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/stroke_inpatient_outpatient.csv
#' @details At least 1 inpatient or 2 outpatient ICD codes:
#'
#' ICD9: "433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91","436.%"
#'
#' ICD10: "I63.%","G46.3","G46.4"
#' @export
stroke_inpatient_outpatient <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    # Pull inpatient and outpatient codes
    icd9_codes <- c("433.01","433.11","433.21","433.31","433.81","433.91","434.01","434.11","434.91","436.%")
    icd10_codes <- c("I63.%","G46.3","G46.4")
    codes <- c(icd9_codes, icd10_codes)
    stroke_inpt <- aou.reader::inpatient_icd_query(codes,anchor_date_table,before,after)
    stroke_outpt <- aou.reader::outpatient_icd_query(codes,anchor_date_table,before,after)
    # Sort by date
    stroke_inpt_sorted <- stroke_inpt[order(condition_start_date)]
    stroke_outpt_sorted <- stroke_outpt[order(condition_start_date)]
    # De-duplicate by date and person id
    stroke_inpt_sorted <- stroke_inpt_sorted[!duplicated(stroke_inpt_sorted, by=c("person_id", "condition_start_date"))]
    stroke_outpt_sorted <- stroke_outpt_sorted[!duplicated(stroke_outpt_sorted, by=c("person_id", "condition_start_date"))]
    # Get counts and dates (2nd date for outpatient since we need two codes)
    stroke_inpt_count <- stroke_inpt_sorted[,.(stroke_inpt_count = length(unique(condition_start_date)),
                                        stroke_inpt_date = condition_start_date[1]),.(person_id)]
    stroke_outpt_count <- stroke_outpt_sorted[,.(stroke_outpt_count = length(unique(condition_start_date)),
                                        stroke_outpt_date = condition_start_date[2]),.(person_id)]
    # Outer join the inpatient and outpatient datasets
    stroke_counts <- merge(stroke_inpt_count,stroke_outpt_count,by="person_id",all.x=TRUE,all.y=TRUE)
    # Definition: If 1 inpatient or 2 outpatient
    stroke_counts[,stroke_inpt_status := ifelse(is.na(stroke_inpt_count),FALSE,stroke_inpt_count >= 1)]
    stroke_counts[,stroke_outpt_status := ifelse(is.na(stroke_outpt_count),FALSE,stroke_outpt_count >= 2)]
    stroke_counts[,stroke_inpatient_outpatient_status := stroke_inpt_status | stroke_outpt_status]
    # Get the entry date (min date)
    stroke_counts[,stroke_inpatient_outpatient_entry_date := pmin(stroke_inpt_date,stroke_outpt_date,na.rm = TRUE)]
    stroke_counts[,stroke_inpatient_outpatient_entry_date := lubridate::as_date(
        ifelse(stroke_inpatient_outpatient_status == FALSE,NA,
            stroke_inpatient_outpatient_entry_date))]
    # Save data to bucket
    stroke_counts <- stroke_counts[,c("person_id","stroke_inpatient_outpatient_entry_date","stroke_inpatient_outpatient_status")]
    stroke_counts <- stroke_counts[stroke_inpatient_outpatient_status == TRUE]
    .write_to_bucket(stroke_counts,output_folder,"stroke_inpatient_outpatient")
}
