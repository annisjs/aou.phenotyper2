#' Minimum Ejection Fraction (best effort)
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table optional data.frame containing columns: person_id, anchor_date.
#'   If provided, the query window is applied around anchor_date using before/after.
#' @param before an integer >= 0
#' @param after an integer >= 0
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @return output_folder/min_ef.csv
#' @details
#' This algorithm uses \code{aou.reader::ef_query()} as a best-effort source for
#' ejection fraction values. Column mapping may need adjustment once EF data
#' availability/structure is fully verified in your environment.
#' @import data.table aou.reader
#' @export
min_ef <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    empty_out <- data.table::data.table(
        person_id = character(),
        min_ef_entry_date = as.Date(character()),
        min_ef_value = numeric()
    )

    ef_raw <- tryCatch(
        aou.reader::ef_query(anchor_date_table, before, after),
        error = function(e) NULL
    )

    if (is.null(ef_raw))
    {
        .write_to_bucket(empty_out, output_folder, "min_ef")
        return(invisible(NULL))
    }

    ef_dt <- data.table::as.data.table(ef_raw)

    if (nrow(ef_dt) == 0 || ncol(ef_dt) < 3)
    {
        .write_to_bucket(empty_out, output_folder, "min_ef")
        return(invisible(NULL))
    }

    # Best-effort normalization based on observed ef_query usage in this repo.
    data.table::setnames(ef_dt, old = names(ef_dt)[1:3], new = c("person_id", "measurement_date", "ef_value"))

    ef_dt <- ef_dt[!is.na(person_id) & !is.na(measurement_date)]

    if (nrow(ef_dt) == 0)
    {
        .write_to_bucket(empty_out, output_folder, "min_ef")
        return(invisible(NULL))
    }

    ef_dt[, measurement_date := as.Date(measurement_date)]
    ef_dt[, ef_value := suppressWarnings(as.numeric(ef_value))]

    # Keep plausible ejection fraction (%) values to remove obvious junk/outliers.
    ef_dt <- ef_dt[
        is.finite(ef_value) &
        ef_value >= 5 &
        ef_value <= 90
    ]

    if (nrow(ef_dt) == 0)
    {
        .write_to_bucket(empty_out, output_folder, "min_ef")
        return(invisible(NULL))
    }

    data.table::setorder(ef_dt, person_id, measurement_date)

    out <- ef_dt[, .(
        min_ef_value = min(ef_value, na.rm = TRUE)
    ), by = .(person_id)]

    out <- merge(ef_dt, out, by = "person_id", all.y = TRUE, allow.cartesian = TRUE)
    out <- out[ef_value == min_ef_value,
               .(min_ef_entry_date = measurement_date[1], min_ef_value = min_ef_value[1]),
               by = .(person_id)]

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "min_ef")
}
