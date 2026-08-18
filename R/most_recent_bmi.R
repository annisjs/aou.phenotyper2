#' Most Recent BMI
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table optional data.frame containing columns: person_id, anchor_date.
#' @param before an integer >= 0
#' @param after an integer >= 0
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @return output_folder/most_recent_bmi.csv
#' @import data.table aou.reader
#' @export
most_recent_bmi <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    result_bmi <- data.table::as.data.table(aou.reader::bmi_query(anchor_date_table, before, after))

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
    result_bmi[, bmi := suppressWarnings(as.numeric(bmi))]

    # Keep plausible BMI values to remove obvious junk/outlier records.
    result_bmi <- result_bmi[
        is.finite(bmi) &
        bmi >= 10 &
        bmi <= 100
    ]

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

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "most_recent_bmi")
}
