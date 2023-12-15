#' Age stopped smoking
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Data is from the AoU intake survey. Do you now smoke cigarettes every day, some days, or not at all?
#' @return output_folder/smoking_stop_age.csv
#' @import data.table stringr aou.reader
#' @export
smoking_stop_age <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query("1585870",anchor_date_table,before,after)
  colnames(result) <- ,c("person_id","smoking_stop_age_value","smoking_stop_age_entry_date")
  .write_to_bucket(result,output_folder,"smoking_stop_age")
}
