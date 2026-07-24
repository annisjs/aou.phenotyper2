#' Mean weight
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table optional data.frame containing columns: person_id, anchor_date.
#'   If provided, results are returned per (person_id, anchor_date). If NULL/empty, results are per person_id.
#' @param before an integer >= 0
#' @param after an integer >= 0
#' @param suffix optional string appended to the end of every output column name except person_id.
#'   Defaults to NULL (no renaming).
#' @return output_folder/mean_weight.csv
#' @import data.table stringr aou.reader bigrquery
#' @export
mean_weight <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
  result_all <- aou.reader::weight_query(anchor_date_table, before, after)
  result_all <- data.table::as.data.table(result_all)

  has_anchor <- !is.null(anchor_date_table) &&
    is.data.frame(anchor_date_table) &&
    nrow(anchor_date_table) > 0 &&
    all(c("person_id", "anchor_date") %in% names(anchor_date_table))

  summarize <- function(dt, bycols) {
    dt[, .(
      mean_weight_value = mean(weight, na.rm = TRUE),
      total_weight_n    = sum(!is.na(weight)),
      weight_unit       = {
        u <- unit_concept_name[!is.na(unit_concept_name)]
        if (length(u) == 0) NA_character_ else u[1]
      }
    ), by = bycols]
  }

  if (has_anchor) {
    result_all <- data.table::as.data.table(
      merge(result_all, anchor_date_table[, c("person_id", "anchor_date")], by = "person_id", all.x = TRUE)
    )
    out <- summarize(result_all, .(person_id, anchor_date))
  } else {
    out <- summarize(result_all, .(person_id))
  }

  if (!is.null(suffix) && nzchar(suffix)) {
    cols_to_rename <- setdiff(names(out), "person_id")
    data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
  }

  .write_to_bucket(out, output_folder, "mean_weight")
}