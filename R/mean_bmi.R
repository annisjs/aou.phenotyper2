#' Mean BMI
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/mean_bmi.csv
#' @import data.table stringr aou.reader
mean_bmi <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{

    result_bmi <- aou.reader::bmi_query(anchor_date_table, before, after)
    result_bmi <- as.data.table(result_bmi)
    colnames(result_bmi) <- c("person_id", "bmi_entry_date", "bmi_value")

    result_bmi <- result_bmi[, .(
        mean_bmi_value = mean(bmi_value, na.rm = TRUE),
        total_bmi_n    = sum(!is.na(bmi_value))
    ), by = .(person_id)]

    .write_to_bucket(result_bmi, output_folder, "mean_bmi")
}