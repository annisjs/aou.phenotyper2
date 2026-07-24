#' Mean diastolic blood pressure and count
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table optional data.frame containing columns: person_id, anchor_date.
#'   If provided, results are returned per (person_id, anchor_date). If NULL/empty, results are per person_id.
#' @param before an integer >= 0
#' @param after an integer >= 0
#' @param suffix optional string appended to the end of every output column name except person_id.
#'   Defaults to NULL (no renaming).
#' @return output_folder/mean_dbp.csv
#' @export
mean_dbp <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
  result_all <- aou.reader::dbp_query(anchor_date_table, before, after)
  result_all <- data.table::as.data.table(result_all)

  has_anchor <- !is.null(anchor_date_table) &&
    is.data.frame(anchor_date_table) &&
    nrow(anchor_date_table) > 0 &&
    all(c("person_id", "anchor_date") %in% names(anchor_date_table))

  if (has_anchor) {
    result_all <- data.table::as.data.table(
      merge(result_all, anchor_date_table[, c("person_id", "anchor_date")], by = "person_id", all.x = TRUE)
    )
    out <- result_all[, .(
      mean_dbp_value = mean(value_as_number, na.rm = TRUE),
      total_dbp_n    = sum(!is.na(value_as_number))
    ), by = .(person_id, anchor_date)]
  } else {
    out <- result_all[, .(
      mean_dbp_value = mean(value_as_number, na.rm = TRUE),
      total_dbp_n    = sum(!is.na(value_as_number))
    ), by = .(person_id)]
  }

  if (!is.null(suffix) && nzchar(suffix)) {
    cols_to_rename <- setdiff(names(out), "person_id")
    data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
  }

  .write_to_bucket(out, output_folder, "mean_dbp")
}