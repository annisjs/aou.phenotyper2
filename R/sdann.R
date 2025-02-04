#' SDANN: Standard deviation of the average NN intervals
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdann.csv
#' @details Heart rate minute level data is extracted and five-minute averages are calculated for consecutive five-minute intervals.
#' The average HR is used to calculate the average RR duration of each five-minute interval:
#' Average RR = 6000 / mean(HR)
#' Subsequently, the standard deviation of all the five-minute RR intervals is calculated, yielding the SDANN value (in ms).
#' @export
sdann <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::sdann_query(anchor_date_table,before,after)
  .write_to_bucket(result,output_folder,"sdann",TRUE,"sdann_query_result.csv")
}
