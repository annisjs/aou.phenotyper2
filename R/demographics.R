#' Demographics
#'
#' @param output_folder the folder to write the output
#' @return output_folder/demographics.csv
#' @export
demographics <- function(output_folder)
{
    result <- aou.reader::demographics_query()
    .write_to_bucket(result,output_folder,"demographics")
}
