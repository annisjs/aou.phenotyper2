#' Heart Rate Summary
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/heart_rate_summary.csv
#' @details
#' Heart rate summary includes for each person_id and date: 
#'  heart_rate_summary_zone_name
#'  heart_rate_summary_min_heart_rate
#'  heart_rate_summary_max_heart_rate 
#'  heart_rate_summary_minute_in_zone
#' @export
heart_rate_summary <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::heart_rate_summary_query(anchor_date_table,before,after)
  .write_to_bucket(result,output_folder,"heart_rate_summary",TRUE,"heart_rate_summary_query.csv")
}
