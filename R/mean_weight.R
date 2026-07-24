#' Mean weight
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined
#'   around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mean_weight.csv
#' @import data.table stringr aou.reader bigrquery
#' @export
mean_weight <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{

  result_all <- aou.reader::weight_query(anchor_date_table, before, after)
  result_all <- as.data.table(merge(result_all, anchor_date_table, by = "person_id"))

  # Optional (kept for consistency with closest_* patterns)
  result_all[, diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]

  # Mean per person (within the query's time window); include count
  result_all <- result_all[, .(
    mean_weight_value = mean(weight, na.rm = TRUE),
    total_weight_n    = sum(!is.na(weight)),
    weight_unit       = {
      u <- unit_concept_name[!is.na(unit_concept_name)]
      if (length(u) == 0) NA_character_ else u[1]
    }
  ), by = .(person_id, anchor_date)]

  .write_to_bucket(result_all, output_folder, "mean_weight")
}