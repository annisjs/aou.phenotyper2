#' Date at Age 18
#'
#' @param output_folder the folder to write the output
#' @return output_folder/date_at_age_18.csv
#' @import data.table
#' @export
date_at_age_18 <- function(output_folder)
{
    dems <- aou.reader::demographics_query()
    dems_dt <- as.data.table(dems)

    # Ensure Date type, then add 18 years
    dems_dt[, date_at_age_18 := as.Date(date_of_birth) + data.table::years(18)]

    result_dt <- dems_dt[, .(person_id, date_at_age_18)]
    .write_to_bucket(result_dt, output_folder, "date_at_age_18")
}