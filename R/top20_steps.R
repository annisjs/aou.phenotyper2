#' Top 20 Steps
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/top20_steps.csv
#' @details Top 20-minute step count for each day.
#' @import stringr bigrquery
#' @export
top20_steps <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::top_steps_query(20,anchor_date_table,before,after)
    .write_to_bucket(result,output_folder,"top20_steps",TRUE,"top_steps_query.csv")
}
