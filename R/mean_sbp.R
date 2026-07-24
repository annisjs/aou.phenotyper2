#' Mean systolic blood pressure and count
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date.
#'   A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mean_sbp.csv
#' @export
mean_sbp <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{

  result_all <- aou.reader::sbp_query(anchor_date_table, before, after)
  result_all <- as.data.table(merge(result_all, anchor_date_table, by = "person_id"))

  # optional: keep the same diff calculation pattern as your closest_sbp codebase
  result_all[, diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]

  # per-subject summary across all SBPs returned in the window
  result_all <- result_all[, .(
    mean_sbp_value = mean(value_as_number, na.rm = TRUE),
    total_sbp_n    = sum(!is.na(value_as_number))
  ), by = .(person_id, anchor_date)]

  .write_to_bucket(result_all, output_folder, "mean_sbp")
}