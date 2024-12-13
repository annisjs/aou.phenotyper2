#' Last asleep datetime
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/last_asleep.csv
#' @details last asleep level, date and time where is_main_sleep = true
#' @export
last_asleep <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    sleep_terms <- "is_main_sleep = 'true' AND level != 'wake' AND level != 'awake'"
    time <- "last"
    result <- aou.reader::sleep_level_query(sleep_terms,time,anchor_date_table,before,after)
    colnames(result) <- c("person_id","last_asleep_date","last_asleep_datetime","last_asleep_duration",
                          "last_asleep_is_main_sleep")
    .write_to_bucket(result,output_folder,"last_asleep")
}
