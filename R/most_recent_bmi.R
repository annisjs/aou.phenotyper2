#' Most Recent BMI
#'
#' @param output_folder the folder to write the output
#' @return output_folder/most_recent_bmi.csv
#' @import data.table aou.reader
#' @export
most_recent_bmi <- function(output_folder)
{
    result_bmi <- data.table::as.data.table(aou.reader::bmi_query())

    if (nrow(result_bmi) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            most_recent_bmi_entry_date = as.Date(character()),
            most_recent_bmi_value = numeric()
        )
        .write_to_bucket(empty, output_folder, "most_recent_bmi")
        return(invisible(NULL))
    }

    result_bmi <- result_bmi[!is.na(person_id) & !is.na(measurement_date)]

    if (nrow(result_bmi) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            most_recent_bmi_entry_date = as.Date(character()),
            most_recent_bmi_value = numeric()
        )
        .write_to_bucket(empty, output_folder, "most_recent_bmi")
        return(invisible(NULL))
    }

    result_bmi[, measurement_date := as.Date(measurement_date)]
    data.table::setorder(result_bmi, person_id, measurement_date)

    out <- result_bmi[, .SD[.N], by = person_id]
    out <- out[, .(
        person_id,
        most_recent_bmi_entry_date = measurement_date,
        most_recent_bmi_value = bmi
    )]

    .write_to_bucket(out, output_folder, "most_recent_bmi")
}