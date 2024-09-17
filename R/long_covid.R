#' Long Covid
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/long_covid.csv
#' @details Uses concept code U09.0:
#'
#' @export
long_covid <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  source_values <- c("U09.9")
  result_all <- aou.reader::condition_query(source_values=source_values,anchor_date_table=anchor_date_table,before=before,after=after)
  result_all <- setDT(result_all)[,.(long_covid_entry_date = min(condition_start_date),
                                     long_covid_status = length(condition_start_date) > 0),
                                  .(person_id)]
  .write_to_bucket(result_all,output_folder,"long_covid")
}
