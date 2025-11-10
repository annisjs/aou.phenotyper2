#' SDOH Survey: Housing instability
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_housing_instability.csv
#' @details This 1-item scale examines how often a survey respondant has moved in the past year:
#' 
#' 40192441: Number of moves in past 12 months
#' Ref: https://www.aamc.org/media/25736/download
#' @export
sdoh_survey_housing_instability <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query(40192441)
  result[, item_score := fcase(survey_response <= 1, 0,
                               survey_response > 1, 1,
                               default = NA)]
  result_agg <- result[, .(sdoh_survey_housing_instability_score = ifelse(any(!is.na(item_score)), 
                                                                    ifelse(item_score == 1, 
                                                                        "Housing instability", 
                                                                        "Hosing stable"),
                                                                    as.character(NA))),
                      .(person_id)]
  .write_to_bucket(result_agg, output_folder, "sdoh_survey_housing_instability")
}