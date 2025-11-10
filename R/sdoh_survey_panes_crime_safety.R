#' SDOH Survey: PANES - Crime and Safety
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_panes_crime_safety.csv
#' @details 
#' 40192414: The crime rate in my neighborhood makes it unsafe to go on walks during the day
#' 40192492: The crime rate in my neighborhood makes it unsafe to go on walks at night
#' @export
sdoh_survey_panes_crime_safety <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query(c(40192492, 40192414))
  result[, panes_item_score := fcase(survey_response == "Strongly agree", 1,
                                     survey_response == "Somewhat agree", 2,
                                     survey_response == "Somewhat disagree", 3,
                                     survey_response == "Strongly disagree", 4,
                                     default = NA)]
  result_agg <- result[, sdoh_survey_panes_crime_safety_score := mean(panes_item_score, na.rm = T), .(person_id)]
  .write_to_bucket(result_agg, output_folder, "sdoh_survey_panes_crime_safety")
}