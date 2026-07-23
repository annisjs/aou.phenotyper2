#' Mean diastolic blood pressure and count
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date.
#'   A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mean_dbp.csv
#' @export
mean_dbp <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{
  if (is.null(anchor_date_table))
  {
    stop("mean_dbp is not a primary variable and requires an anchor date table.")
  }

  result_all <- aou.reader::dbp_query(anchor_date_table, before, after)
  result_all <- as.data.table(merge(result_all, anchor_date_table, by = "person_id"))

  # optional: keep the same diff calculation pattern as your closest_dbp codebase
  result_all[, diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]

  # per-subject summary across all dbps returned in the window
  result_all <- result_all[, .(
    mean_dbp_value = mean(value_as_number, na.rm = TRUE),
    total_dbp_n    = sum(!is.na(value_as_number))
  ), by = .(person_id, anchor_date)]

  .write_to_bucket(result_all, output_folder, "mean_dbp")
}