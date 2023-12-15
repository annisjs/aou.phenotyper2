#' First wake datetime
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/first_wake_datetime.csv
#' @details First sleep level = "wake", date and time
#' @import stringr bigrquery
#' @export
first_wake <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    sleep_terms <- "level = 'wake' AND is_main_sleep = 'true'"
    result <- aou.reader::sleep_level_query(sleep_terms,anchor_date_table,before,after)
    colnames(result) <- c("person_id","first_wake_date","first_wake_datetime",
                "first_wake_duration","first_wake_is_main_sleep")
    .write_to_bucket(result,output_folder,"first_wake")
}
