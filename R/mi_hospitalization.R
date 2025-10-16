#' MI hospitalization first date
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mi_hospitalization.csv
#' @export
mi_hospitalization <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd_codes <- c("410","410.%","411","411.%","I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%")
    result <- aou.reader::hospitalization_query(icd_codes,anchor_date_table,before,after)
    result <- result[,c("person_id","hospitalization_entry_date")]
    result <- result[order(hospitalization_entry_date)]
    result <- result[,.(mi_hospitalization_entry_date = hospitalization_entry_date[1],
                        mi_hospitalization_status = TRUE), .(person_id)]
    .write_to_bucket(result,output_folder,"mi_hospitalization")
}
