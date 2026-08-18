#' Most Recent HgbA1c
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table optional data.frame containing columns: person_id, anchor_date.
#' @param before an integer >= 0
#' @param after an integer >= 0
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @details Searches for
#'
#' "Hemoglobin A1c/Hemoglobin.total in Blood by Electrophoresis"
#'
#' "Hemoglobin A1c/Hemoglobin.total in Blood by calculation"
#'
#' "Hemoglobin A1c/Hemoglobin.total in Blood by IFCC protocol"
#'
#' "Hemoglobin A1c/Hemoglobin.total in Blood"
#'
#' "Hemoglobin A1c/Hemoglobin.total in Blood by HPLC"
#'
#' @return output_folder/most_recent_hgba1c.csv
#' @import data.table aou.reader
#' @export
most_recent_hgba1c <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    lab_terms <- c("Hemoglobin A1c/Hemoglobin.total in Blood by Electrophoresis",
                   "Hemoglobin A1c/Hemoglobin.total in Blood by calculation",
                   "Hemoglobin A1c/Hemoglobin.total in Blood by IFCC protocol",
                   "Hemoglobin A1c/Hemoglobin.total in Blood",
                   "Hemoglobin A1c/Hemoglobin.total in Blood by HPLC")

    result_hgba1c <- data.table::as.data.table(aou.reader::lab_query(lab_terms, anchor_date_table, before, after))

    if (nrow(result_hgba1c) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            most_recent_hgba1c_entry_date = as.Date(character()),
            most_recent_hgba1c_value = numeric()
        )
        .write_to_bucket(empty, output_folder, "most_recent_hgba1c")
        return(invisible(NULL))
    }

    result_hgba1c <- result_hgba1c[!is.na(person_id) & !is.na(measurement_date)]
    result_hgba1c[, value_as_number := suppressWarnings(as.numeric(value_as_number))]

    # Keep only plausible HbA1c percent values to remove obvious junk/outlier records.
    result_hgba1c <- result_hgba1c[
        is.finite(value_as_number) &
        value_as_number >= 1 &
        value_as_number <= 20
    ]

    if (nrow(result_hgba1c) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            most_recent_hgba1c_entry_date = as.Date(character()),
            most_recent_hgba1c_value = numeric()
        )
        .write_to_bucket(empty, output_folder, "most_recent_hgba1c")
        return(invisible(NULL))
    }

    result_hgba1c[, measurement_date := as.Date(measurement_date)]
    data.table::setorder(result_hgba1c, person_id, measurement_date)

    out <- result_hgba1c[, .SD[.N], by = person_id]
    out <- out[, .(
        person_id,
        most_recent_hgba1c_entry_date = measurement_date,
        most_recent_hgba1c_value = value_as_number
    )]

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "most_recent_hgba1c")
}