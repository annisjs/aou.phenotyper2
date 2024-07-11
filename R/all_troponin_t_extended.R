#' All Troponin T with Units and Range High Columns
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#'
#' "Troponin T.cardiac in Serum or Plasma with  unit and range high column"
#' @return output_folder/all_troponin_t.csv
#' @export
all_troponin_t_extended <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result_all <- aou.reader::lab_query_extended("Troponin T.cardiac [Mass/volume] in Serum or Plasma", ext_cols =c("range_high", "unit_source_value"),
                                               anchor_date_table,before,after)
  colnames(result_all) <- c("person_id","all_troponin_t_entry_date","all_troponin_t_value", "all_troponin_t_range_high", "all_troponin_t_unit_source_value")
  .write_to_bucket(result_all,output_folder,"all_troponin_t_extended")
}