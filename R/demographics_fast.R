#' Demographics (fast, experimental)
#'
#' @param output_folder the folder to write the output
#' @return output_folder/demographics_fast.csv
#' @details Caches the demographics query for the current R session so repeated
#' calls do not download the same person-level table from BigQuery.
#' @import data.table aou.reader
#' @export
demographics_fast <- function(output_folder)
{
    if (is.null(.demographics_fast_cache$data))
    {
        .demographics_fast_cache$data <- data.table::as.data.table(
            aou.reader::demographics_query()
        )
    }

    result <- data.table::copy(.demographics_fast_cache$data)
    .write_to_bucket(result, output_folder, "demographics_fast")
}

.demographics_fast_cache <- new.env(parent = emptyenv())
