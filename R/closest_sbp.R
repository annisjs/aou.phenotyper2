#' Closest systolic blood pressure
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/closest_sbp.csv
#' @export
closest_sbp <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    if (is.null(anchor_date_table))
    {
        stop("closest_sbp is not a primary variable and requires an anchor date table.")
    }
    result_all <- aou.reader::sbp_query(anchor_date_table,before,after)
    result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
    result_all <- result_all[order(diff)]
    result_all <- result_all[,.(closest_sbp_entry_date = measurement_date[1],
                                closest_sbp_value = value_as_number[1]),
                                .(person_id,anchor_date)]
    .write_to_bucket(result_all,output_folder,"closest_sbp")
}
