#' Fitbit
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/fitbit.csv
#' @details Fitbit data contains "person_id","date","steps",
#' "fairly_active_minutes","fairly_active_minutes","lightly_active_minutes",
#' "sedentary_minutes","very_active_minutes"
#' @export
fitbit <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::fitbit_query(anchor_date_table,before,after)
    .write_to_bucket(result,output_folder,"fitbit",TRUE,"fitbit_query.csv")
}
