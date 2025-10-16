#' All CAD hospitalizations
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_cad_hospitalizations.csv
#' @export
all_cad_hospitalizations <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd_codes <- c("410","410.%","411","411.%","412","412.%","413","413.%","414","414.%","V45.82","I25.1%")
    result <- aou.reader::hospitalization_query(icd_codes,anchor_date_table,before,after)
    result <- result[,c("person_id","hospitalization_entry_date")]
    result[,all_cad_hospitalization_entry_date := hospitalization_entry_date]
    result <- result[,c("person_id","all_cad_hospitalizations_entry_date")]
    result <- result[!duplicated(result)]
    .write_to_bucket(result,output_folder,"all_cad_hospitalizations")
}
