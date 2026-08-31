#' HFpEF using ICD codes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hfpef_icd.csv
#' @details At least 1 ICD code
#'
#' Inclusion: 1 inpatient or 2 outpatient
#' ICD-9 Codes
#'  •	428.30 Unspecified diastolic heart failure
#'  •	428.31 Acute diastolic heart failure
#'  •	428.32 Chronic diastolic heart failure
#'  •	428.33 Acute on chronic diastolic heart failure
#'  ICD-10 Codes
#'  •	I50.30 Unspecified diastolic heart failure
#'  •	I50.31 Acute diastolic heart failure
#'  •	I50.32 Chronic diastolic heart failure
#'  •	I50.33 Acute on chronic diastolic heart failure
#'  Exclusion Criterion: HF reduced EF 1 inpatient or 2 outpatient
#'  ICD-9 Codes
#'  •	428.20 Unspecified systolic heart failure
#'  •	428.21 Acute systolic heart failure
#'  •	428.22 Chronic systolic heart failure
#'  •	428.23 Acute on chronic systolic heart failure
#'  ICD-10 Codes
#'  •	I50.20 Unspecified systolic heart failure
#'  •	I50.21 Acute systolic heart failure
#'  •	I50.22 Chronic systolic heart failure
#'  •	I50.23 Acute on chronic systolic heart failure
#' @export
#'
hfpef_icd <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{
    icd_codes <- c("428.30", "428.31", "428.32", "428.33",
                    "I50.30", "I50.31", "I50.32", "I50.33")
    icd_ex_codes <- c("428.20", "428.21", "428.22", "428.23",
                      "I50.20", "I50.21", "I50.22", "I50.23")

    icd_inpatient_dat <- aou.reader::inpatient_icd_query(icd_codes)
    icd_inpatient_ex_dat <- aou.reader::inpatient_icd_query(icd_ex_codes)

    icd_outpatient_dat <- aou.reader::outpatient_icd_query(icd_codes)
    icd_outpatient_ex_dat <- aou.reader::outpatient_icd_query(icd_ex_codes)

    icd_inpatient_dat[, type := "include"]
    icd_inpatient_ex_dat[, type := "exclude"]
    icd_inpatient_dat <- rbind(icd_inpatient_dat, icd_inpatient_ex_dat)
    icd_inpatient_dat <- icd_inpatient_dat[!duplicated(icd_inpatient_dat)]
    icd_inpatient_dat <- icd_inpatient_dat[order(condition_start_date)]
    icd_inpatient_dat <- icd_inpatient_dat[, .(hfpef_date = condition_start_date[1]),
                                           .(person_id, type)]

    icd_outpatient_dat[, type := "include"]
    icd_outpatient_ex_dat[, type := "exclude"]
    icd_outpatient_dat <- rbind(icd_outpatient_dat, icd_outpatient_ex_dat)
    icd_outpatient_dat <- icd_outpatient_dat[!duplicated(icd_outpatient_dat)]
    icd_outpatient_dat <- icd_outpatient_dat[order(condition_start_date)]
    icd_outpatient_dat <- icd_outpatient_dat[, .(hfpef_date = condition_start_date[2]),
                                             .(person_id, type)]

    icd_dat <- rbind(icd_inpatient_dat, icd_outpatient_dat) 
    icd_dat <- icd_dat[!duplicated(icd_dat)]
    icd_dat <- icd_dat[order(hfpef_date)]
    icd_dat <- icd_dat[, .(hfpef_icd_entry_date = hfpef_date[1]), .(person_id, type)]

    icd_dat[, has_exclusion := any(type == "exclude"), .(person_id)]
    icd_dat <- icd_dat[has_exclusion == FALSE]
   
    icd_dat[, hfpef_icd_status := TRUE]
    icd_dat <- icd_dat[, c("person_id", "hfpef_icd_entry_date", "hfpef_icd_status")]
    .write_to_bucket(icd_dat, output_folder, "hfpef_icd")
}


