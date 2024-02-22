#' Sleep data from Fitbit
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sleep.csv
#' @details Fitbit sleep data contains:
#' 	person_id
#' 	sleep_date
#' 	is_main_sleep
#' 	minute_in_bed
#' 	minute_a_sleep
#' 	minute_after_wakeup
#' 	minute_awake
#' 	minute_restless
#' 	minute_deep
#' 	minute_light
#' 	minute_rem
#' 	minute_wake
#' @import stringr bigrquery
#' @export
sleep <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::sleep_query(anchor_date_table,before,after)
  .write_to_bucket(result,output_folder,"sleep")
}
