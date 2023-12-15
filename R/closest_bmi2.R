#' Closest BMI (Using only the BMI measure)
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/closest_bmi.csv
#' @details In closest_bmi algorithm, height and weight are used when BMI is missing. closest_bmi2 only uses the BMI measurement.
closest_bmi2 <-  function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    if (is.null(anchor_date_table))
    {
    stop("closest_bmi is not a primary variable and requires an anchor date table.")
    }
    result_bmi <- aou.reader::bmi_query(anchor_date_table,before,after)
    result_bmi <- as.data.table(merge(result_bmi,anchor_date_table,by="person_id",allow.cartesian=TRUE))
    result_bmi[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
    result_bmi <- result_bmi[order(diff)]
    result_bmi <- result_bmi[,.(closest_bmi2_entry_date = measurement_date[1],
                                closest_bmi2_value = bmi[1]),
                            .(person_id,anchor_date)]
    .write_to_bucket(result_bmi,output_folder,"closest_bmi2")
}
