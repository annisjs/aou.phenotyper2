#' Approximate resting heart rate
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/approx_resting_heart_rate.csv
#' @details Looks for HR when there is no significant steps/movemet for 10 mins, and then takes 10th percentile of that set of observed values.
#' @export
approx_resting_heart_rate <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL,cohort=NULL)
{
  result <- aou.reader::approx_resting_heart_rate_query(anchor_date_table,before,after,cohort)
  .write_to_bucket(result,output_folder,"approx_resting_heart_rate")
}
