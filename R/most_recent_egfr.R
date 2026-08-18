#' Most Recent eGFR
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table optional data.frame containing columns: person_id, anchor_date.
#' @param before an integer >= 0
#' @param after an integer >= 0
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @details Searches for
#'
#' "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)"
#'
#' "Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)"
#'
#' "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)"
#'
#' "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood"
#'
#' "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)"
#'
#' "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)"
#'
#' @return output_folder/most_recent_egfr.csv
#' @import data.table aou.reader
#' @export
most_recent_egfr <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    lab_terms <- c("Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood",
                   "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)",
                   "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)")

    result_egfr <- data.table::as.data.table(aou.reader::lab_query(lab_terms, anchor_date_table, before, after))

    if (nrow(result_egfr) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            most_recent_egfr_entry_date = as.Date(character()),
            most_recent_egfr_value = numeric()
        )
        .write_to_bucket(empty, output_folder, "most_recent_egfr")
        return(invisible(NULL))
    }

    result_egfr <- result_egfr[!is.na(person_id) & !is.na(measurement_date)]
    result_egfr[, value_as_number := suppressWarnings(as.numeric(value_as_number))]
    result_egfr <- result_egfr[
        !is.na(value_as_number) &
        is.finite(value_as_number) &
        value_as_number >= 1 &
        value_as_number <= 200
    ]

    if (nrow(result_egfr) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            most_recent_egfr_entry_date = as.Date(character()),
            most_recent_egfr_value = numeric()
        )
        .write_to_bucket(empty, output_folder, "most_recent_egfr")
        return(invisible(NULL))
    }

    result_egfr[, measurement_date := as.Date(measurement_date)]
    data.table::setorder(result_egfr, person_id, measurement_date)

    out <- result_egfr[, .SD[.N], by = person_id]
    out <- out[, .(
        person_id,
        most_recent_egfr_entry_date = measurement_date,
        most_recent_egfr_value = value_as_number
    )]

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "most_recent_egfr")
}
