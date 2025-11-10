#' SDOH Survey: Social cohesion
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_social_cohesion.csv
#' @details This 4-item scale evaluates the perceptions of neighborhood characteristics:
#'
#' 40192411: People in your neighborhood generally get along with each other
#' 40192417: People in your neighborhood share the same values
#' 40192463: People around here are willing to help their neighbor
#' 40192499: People in your neighborhood can be trusted
#' Ref: Mahasin S. Mujahid, Ana V. Diez Roux, Jeffrey D. Morenoff, Trivellore Raghunathan, Assessing the Measurement Properties of Neighborhood Scales: From Psychometrics to Ecometrics, American Journal of Epidemiology, Volume 165, Issue 8, 15 April 2007, Pages 858–867, https://doi.org/10.1093/aje/kwm040
#' @export
sdoh_survey_social_cohesion <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query(c(40192411, 40192417, 40192463, 40192499))
  result[, item_score := fcase(survey_response == "Strongly agree", 1,
                                     survey_response == "Agree", 2,
                                     survey_response == "Neutral (neither agree nor disagree)", 3,
                                     survey_response == "Disagree", 4,
                                     survey_response == "Strongly disagree", 5,
                                     default = NA)]
  result_agg <- result[, sdoh_survey_social_cohesion_score := mean(item_score, na.rm = T), .(person_id)]
  .write_to_bucket(result_agg, output_folder, "sdoh_survey_social_cohesion")
}