#' All Weight
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/all_weight.csv
#' @import data.table stringr aou.reader
all_weight <-  function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result_weight <- aou.reader::weight_query(anchor_date_table,before,after)
    colnames(result_weight) <- c("person_id","all_weight_entry_date","all_weight_value")
    .write_to_bucket(result_weight,output_folder,"all_weight")
}
