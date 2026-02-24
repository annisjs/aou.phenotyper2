#' Average step count over 5 minute intervals
#'
#' @param output_folder the folder to write the output
#' @return output_folder/sdann.csv
#' @details Average step count over all available 5 minute intervals in a given day.
#' @export
step_count_average_5_minute_interval <- function(output_folder)
{
  result <- aou.reader::step_count_average_5_minute_interval_query()
  .write_to_bucket(result,output_folder,"step_count_average_5_minute_interval",TRUE,"step_count_average_5_minute_interval_query_result.csv")
}
