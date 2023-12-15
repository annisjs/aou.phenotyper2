#' Whole Genome Sequencing Flag
#'
#' @param output_folder the folder to write the output
#' @return output_folder/wgs.csv
#' @details wgs.csv will contain:
#' 	person_id
#'  wgs_status
#' @export
wgs <- function(output_folder)
{
    result <- aou.reader::wgs_query()
    result[,wgs_status := TRUE]
    .write_to_bucket(result,output_folder,"wgs")
}
