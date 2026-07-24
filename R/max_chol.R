#' Max Cholesterol
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#'
#' "Cholesterol [Mass/volume] in Serum or Plasma"
#' @return output_folder/max_chol.csv
#' @export
max_chol <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{

    lab_terms <- "Cholesterol [Mass/volume] in Serum or Plasma"
    result_all <- aou.reader::lab_query(lab_terms, anchor_date_table, before, after)
    result_all <- as.data.table(merge(result_all, anchor_date_table, by = "person_id"))

    # max within the query's time window
    result_all <- result_all[, .(
        max_chol_value = suppressWarnings(max(value_as_number, na.rm = TRUE)),
        total_chol_n   = sum(!is.na(value_as_number))
    ), by = .(person_id, anchor_date)]

    # if a subject has only NA values, max(..., na.rm=TRUE) returns -Inf
    result_all[is.infinite(max_chol_value), max_chol_value := NA_real_]

    .write_to_bucket(result_all, output_folder, "max_chol")
}