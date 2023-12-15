#' Number of alcohol drinks
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/alcohol_drinks.csv
#' @details Alcohol AoU intake survey. How often did you have a drink containing alcohol in the past year?
#' @export
alcohol_drinks <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1586201",anchor_date_table,before,after)
    colnames(result) <- c("person_id","alcohol_drinks_status","alcohol_drinks_entry_date")
    .write_to_bucket(result,output_folder,"alcohol_drinks")
}

