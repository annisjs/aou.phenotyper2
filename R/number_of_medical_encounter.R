#' Number of Medical Encounters
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/number_of_medical_encounters.csv
#' @import stringr bigrquery data.table
#' @export
number_of_medical_encounter <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::medical_encounter_query("count",anchor_date_table,before,after)
  colnames(result) <- c("person_id","number_of_medical_encounter_count")
  .write_to_bucket(result,output_folder,"number_of_medical_encounter")
}
