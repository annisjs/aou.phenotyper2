#' Primary consent date
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/primary_consent_date.csv
#' @details The date someone enrolled into the All of Us Research Program as a data contributing participant
#' @export
primary_consent_date <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result_all <- aou.reader::primary_consent_date_query(anchor_date_table,before,after)
    .write_to_bucket(result_all,output_folder,"primary_consent_date")
}
