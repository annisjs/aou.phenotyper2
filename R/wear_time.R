#' Fitbit wear time
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details An hour of wear time is defined when step count is > 0 for a given hour of the day,
#' @return output_folder/wear_time.csv
#' @import data.table stringr aou.reader bigrquery
#' @export
wear_time <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::wear_time_query(anchor_date_table,before,after)
  .write_to_bucket(result,output_folder,"wear_time",TRUE,"wear_time_query.csv")
}
