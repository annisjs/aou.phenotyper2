#' Summary Triglycerides
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @return output_folder/summary_trigs.csv
#' @details Searches for
#'
#' "Triglyceride [Mass/volume] in Serum or Plasma",
#'
#' "Triglyceride [Mass/volume] in Blood"
#' @import data.table aou.reader
#' @export
summary_trigs <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    lab_terms <- c("Triglyceride [Mass/volume] in Serum or Plasma",
                   "Triglyceride [Mass/volume] in Blood")

    result_all <- data.table::as.data.table(aou.reader::lab_query(lab_terms, anchor_date_table, before, after))

    if (nrow(result_all) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            summary_trigs_median = numeric(),
            summary_trigs_min = numeric(),
            summary_trigs_max = numeric()
        )
        .write_to_bucket(empty, output_folder, "summary_trigs")
        return(invisible(NULL))
    }

    result_all <- result_all[!is.na(person_id)]
    result_all[, value_as_number := suppressWarnings(as.numeric(value_as_number))]
    # Keep plausible triglyceride values (mg/dL) to remove obvious junk/outliers.
    result_all <- result_all[
        is.finite(value_as_number) &
        value_as_number >= 20 &
        value_as_number <= 3000
    ]

    if (nrow(result_all) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            summary_trigs_median = numeric(),
            summary_trigs_min = numeric(),
            summary_trigs_max = numeric()
        )
        .write_to_bucket(empty, output_folder, "summary_trigs")
        return(invisible(NULL))
    }

    out <- result_all[, .(
        summary_trigs_median = stats::median(value_as_number, na.rm = TRUE),
        summary_trigs_min = min(value_as_number, na.rm = TRUE),
        summary_trigs_max = max(value_as_number, na.rm = TRUE)
    ), by = .(person_id)]

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "summary_trigs")
}
