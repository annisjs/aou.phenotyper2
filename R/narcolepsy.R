#' Narcolepsy
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/narcolepsy.csv
#' @details At least 1 ICD code:
#'
#' ICD-10:
#' G47.41: Narcolepsy with cataplexy
#' G47.411: Narcolepsy with cataplexy
#' G47.419: Narcolepsy without cataplexy
#' G47.42: Narcolepsy in conditions classified elsewhere
#' G47.421: Narcolepsy in conditions classified elsewhere with cataplexy
#' G47.429: Narcolepsy in conditions classified elsewhere without cataplexy 
#' 
#' ICD-9:
#' 347.0: This code specifically covers narcolepsy, a medical classification under "Other Disorders of the Central Nervous System" (340-349). 
#'
#' 347.01: This code specifically covers narcolepsy with cataplexy 
#' 347.00: This code specifically covers narcolepsy without cataplexy 
#' @export
narcolepsy <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("347.0","347.01","347.00")
    icd10_codes <- c("G47.41","G47.411","G47.419","G47.42","G47.421","G47.429")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    result_all <- result_all[order(condition_start_date)]
    result_all <- result_all[,.(narcolepsy_entry_date = condition_start_date[1],
                                narcolepsy_status = TRUE),
                                .(person_id)]
    .write_to_bucket(result_all,output_folder,"narcolepsy")
}
