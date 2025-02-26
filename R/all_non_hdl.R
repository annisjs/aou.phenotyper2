#' All Non-HDL
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/all_non_hdl.csv
#' @import data.table stringr aou.reader
all_non_hdl <-  function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    lab_terms <- c("Cholesterol non HDL [Mass/volume] in Serum or Plasma")
    result_all <- aou.reader::lab_query(lab_terms,anchor_date_table,before,after)
    result_all <- result_all[, c("person_id","measurement_date","value_as_number")]
    setnames(result_all, c("measurement_date","value_as_number"), c("all_non_hdl_entry_date","all_non_hdl_value"))
    .write_to_bucket(result_all,output_folder,"all_non_hdl")
}
