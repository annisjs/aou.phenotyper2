#' SDOH Survey: Perceived stress
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_perceived_stress.csv
#' @details Cohen Perceived Stress Scale, a metric which measures how well participants handled their thoughts and feelings related to stress over the previous month:
#' 40192381: How often have you felt that you were unable to control the important things in your life?
#' 40192396: How often have you been angered because of things that were outside of your control?
#' 40192419: How often have you felt confident about your ability to handle your personal problems?
#' 40192445: How often have you felt that you were on top of things?
#' 40192449: How often have you been able to control irritations in your life?
#' 40192452: How often have you been upset because of something that happened unexpectedly?
#' 40192462: How often have you felt difficulties were piling up so high that you could not overcome them?
#' 40192491: How often have you felt nervous and stressed?
#' 40192506: How often have you found that you could not cope with all the things that you had to do?
#' 40192525: How often have you felt that things were going your way?
#' Ref: Cohen S, Kamarck T, Mermelstein R. A global measure of perceived stress. J Health Soc Behav. 1983 Dec;24(4):385-96. PMID: 6668417 
#' @export
sdoh_survey_perceived_stress <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query(c(40192381,
                                       40192396,
                                       40192452,
                                       40192462,
                                       40192491,
                                       40192506),
                                    anchor_date_table,
                                    before,
                                    after)
  result_rev <- aou.reader::survey_query(c(40192419, 40192445, 40192449, 40192525))
  result[, item_score := fcase(survey_response == "Very Often", 4,
                               survey_response == "Fairly Often", 3,
                               survey_response == "Sometimes", 2,
                               survey_response == "Almost Never", 1,
                               survey_response == "Never", 0,
                               default = NA)]
  result_rev[, item_score := fcase(survey_response == "Very Often", 0,
                               survey_response == "Fairly Often", 1,
                               survey_response == "Sometimes", 2,
                               survey_response == "Almost Never", 3,
                               survey_response == "Never", 4,
                               default = NA)]
  result <- rbind(result, result_rev)
  result_agg <- result[, .(sdoh_survey_perceived_stress_score = sum(item_score, na.rm = T),
                           sdoh_survey_perceived_stress_date = survey_date[1],
                           n_responses = length(which(!is.na(item_score)))),
                           .(person_id)]
  result_agg <- result_agg[n_responses == 10]
  result_agg[, n_responses := NULL]
  .write_to_bucket(result_agg, output_folder, "sdoh_survey_perceived_stress")
}
