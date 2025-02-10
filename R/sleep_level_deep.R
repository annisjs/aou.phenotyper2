#' Deep sleep level data from Fitbit
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sleep_level_deep.csv
#' @details Fitbit sleep data contains:
#' 	person_id
#' 	sleep_date
#' 	start_datetime
#' 	is_main_sleep
#' 	duration_in_min
#' @export
sleep_level_deep <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    sleep_terms <- "level = 'deep'"
    result <- aou.reader::sleep_level_query(sleep_terms,"all",anchor_date_table,before,after)
    colnames(result) <- c('person_id',
                        'sleep_level_deep_date',
                        'sleep_level_deep_start_datetime',
                        'sleep_level_deep_duration_in_min',
                        'sleep_level_deep_is_main_sleep')
  .write_to_bucket(result,output_folder,"sleep_level_deep",TRUE,"sleep_level_query_result.csv")
}
