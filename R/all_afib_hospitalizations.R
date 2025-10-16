#' All AFIB hospitalizations
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_afib_hospitalizations.csv
#' @export
all_afib_hospitalizations <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd_codes <- c("427.3","427.31","427.32","I48","I48.0","I48.1","I48.2","I48.3","I48.4","I48.9","I48.91","I48.92")
    result <- aou.reader::hospitalization_query(icd_codes,anchor_date_table,before,after)
    result <- result[,c("person_id","hospitalization_entry_date")]
    result[,all_afib_hospitalizations_entry_date := hospitalization_entry_date]
    result <- result[,c("person_id","all_afib_hospitalizations_entry_date")]
    result <- result[!duplicated(result)]
    .write_to_bucket(result,output_folder,"all_afib_hospitalizations")
}
