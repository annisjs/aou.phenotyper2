#' Hourly steps count at each day
#'
#' @param output_folder the folder to write the output
#' @param cohort a vector of ids to limit the query
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' 
#' @details Step counts at each day and each hour
#' @return output_folder/hourly_steps_count.csv
#' @import data.table stringr aou.reader bigrquery
#' @export
hourly_steps_count <- function(output_folder, cohort=NULL,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::hourly_steps_query(anchor_date_table=anchor_date_table,before=before,after=after,cohort = cohort)
  .write_to_bucket(result,output_folder,"hourly_steps_count")
}
