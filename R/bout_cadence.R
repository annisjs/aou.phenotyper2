#' Bout Cadence
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @param min_steps a numeric value indicating the minimum number of steps per minute to be included in a bout. Default is 60.
#' @return output_folder/bout_cadence.csv
#' @details
#' Bout cadence is the average steps per minute when the step count >/= `min_steps` steps a minute for at least 2 minutes.
#' The wearer will usually have many of these “bouts” throughout the day.
#' We take the average over the entire day to get the average bout cadence.
#' @export
bout_cadence <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL,min_steps=60)
{
  result <- aou.reader::bout_cadence_query(anchor_date_table,before,after,min_steps)
  .write_to_bucket(result,output_folder,"bout_cadence",TRUE,"bout_cadence_query_result.csv")
}
