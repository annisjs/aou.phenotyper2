#' Deprivation index
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/deprivation_index.csv
#' @details Deprivation index is a composite of many highly correlated measures, e.g., median household income and educational attainment
#' @import stringr bigrquery aou.reader
#' @export
deprivation_index <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::deprivation_index_query(anchor_date_table,before,after)
    .write_to_bucket(result,output_folder,"deprivation_index")
}

