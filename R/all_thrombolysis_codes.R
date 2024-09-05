#' All thrombolysis codes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_thrombolysis_codes.csv
#' CPT: "92977","92975","1012986"
#' @export
all_thrombolysis_codes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    cpt_codes <- c("92977","92975","1012986")
    result_cpt <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
    colnames(result_cpt) <- c("person_id","all_thrombolysis_codes_value","all_thrombolysis_codes_entry_date")
    .write_to_bucket(result_cpt,output_folder,"all_thrombolysis_codes")
}