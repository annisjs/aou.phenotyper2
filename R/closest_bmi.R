#' Closest BMI
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/closest_bmi.csv
closest_bmi <-  function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    if (is.null(anchor_date_table))
    {
    stop("closest_bmi is not a primary variable and requires an anchor date table.")
    }
    result_bmi <- aou.reader::bmi_query(anchor_date_table,before,after)

    result_height <- aou.reader::height_query(anchor_date_table,before,after)
    result_height <- result_height[!duplicated(result_height)]

    result_weight <- aou.reader::weight_query(anchor_date_table,before,after)
    result_weight <- result_weight[!duplicated(result_weight)]

    # Compute BMI from height and weight
    result_hw <- merge(result_height,result_weight,by=c("person_id","measurement_date"),allow.cartesian=TRUE)
    result_hw[, bmi := weight / (height/100)^2]
    result_hw <- result_hw[,c("person_id","measurement_date","bmi")]

    result_all <- rbind(result_bmi,result_hw)
    result_all <- merge(result_all,anchor_date_table,by="person_id",allow.cartesian=TRUE)

    result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
    result_all <- result_all[order(diff)]
    result_all <- result_all[,.(closest_bmi_entry_date = measurement_date[1],
                                closest_bmi_value = bmi[1]),
                            .(person_id,anchor_date)]
    .write_to_bucket(result_all,output_folder,"closest_bmi")
}
