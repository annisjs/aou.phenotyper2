#' Demographics Robust
#'
#' @param output_folder the folder to write the output
#' @return output_folder/demographics_robust.csv
#' @export
demographics_robust <- function(output_folder)
{
    result <- aou.reader::demographics_robust_query()
    .write_to_bucket(result,output_folder,"demographics_robust")
}
