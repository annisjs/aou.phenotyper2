#' All Meds by Cohort
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/anti_hypertensives.csv
#' @details Returns all meds for given cohort
#'
#' @export
all_meds_by_cohort <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL, cohort=NULL)
{
  if (is.null(cohort)){
    stop("Cohort cannot be NULL!")
  }
  meds <- aou.reader::med_query(meds=NULL,anchor_date_table,before,after, cohort=cohort)
  .write_to_bucket(meds,output_folder,"all_meds_by_cohort")
}
