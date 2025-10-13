#' All fasting glucose
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#'
#' Fasting glucose \\[Mass/volume] in Serum or Plasma
#' Fasting glucose \\[Moles/volume] in Blood
#' @return output_folder/all_fasting_glucose.csv
#' @export
all_fasting_glucose <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result_all <- aou.reader::lab_query(c("Fasting glucose [Mass/volume] in Serum or Plasma",
                       "Fasting glucose [Moles/volume] in Blood"),
                        anchor_date_table,before,after)
    colnames(result_all) <- c("person_id","all_fasting_glucose_entry_date","all_fasting_glucose_value")
    .write_to_bucket(result_all,output_folder,"all_fasting_glucose")
}