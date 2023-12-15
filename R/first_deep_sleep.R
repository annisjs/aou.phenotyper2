#' First deep sleep datetime
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/first_deep_sleep.csv
#' @details First deep sleep level, date and time
#' @export
first_deep_sleep <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    sleep_terms <- "level = 'deep' AND is_main_sleep = 'true'"
    result <- aou.reader::sleep_level_query(sleep_terms,output_folder,before,after)
    colnames(result) <- c("person_id","first_deep_sleep_date","first_deep_sleep_datetime",
                "first_deep_sleep_duration","first_deep_sleep_is_main_sleep")
    .write_to_bucket(result,output_folder,"first_deep_sleep")
}
