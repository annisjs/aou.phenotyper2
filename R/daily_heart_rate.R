#' Daily heart rate average and standard deviation at each day
#'
#' @param output_folder the folder to write the output
#' @param cohort a vector of ids to limit the query
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' 
#' @details Average and standard deviation of heart rate at each day
#' @return output_folder/daily_heart_rate.csv
#' @import data.table stringr aou.reader bigrquery
#' @export
daily_heart_rate <- function(output_folder, anchor_date_table=NULL, before=NULL, after=NULL, cohort=NULL)
{
  result <- aou.reader::daily_heart_rate_query(anchor_date_table=anchor_date_table, before=before, after=after, cohort=cohort)
  .write_to_bucket(result,output_folder,"daily_heart_rate",TRUE,"daily_heart_rate_query.csv")
}
