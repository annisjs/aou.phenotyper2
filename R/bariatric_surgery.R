#' Bariatric surgery
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/bariatric_surgery.csv
#' @details At least 1 ICD code
#'
#' ICD9: "V22.%","V23.%"
#'
#' ICD10: "Z33.%"
#' @export
bariatric_surgery <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    cpt_codes <- c("43644","43645","43775","43845","43846","43847")
    cpt_df <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
    cpt_df <- cpt_df[order(entry_date)]
    cpt_df <- cpt_df[,.(bariatric_surgery_status = TRUE,
                        bariatric_surgery_entry_date = entry_date[1]),
                      .(person_id)]
    .write_to_bucket(cpt_df,output_folder,"bariatric_surgery")
}