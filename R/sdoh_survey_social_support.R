#' SDOH Survey: Social support
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_social_support.csv
#' @details The following eight questions evaluate the supportive relationships that survey participants have in their lives:
#'
#' 40192388: How often do you have someone to prepare your meals if you were unable to do it yourself?
#' 40192399: How often do you have someone who understands your problems?
#' 40192439: How often do you have someone to have a good time with?
#' 40192442: How often do you have someone to help you if you were confined to bed?
#' 40192446: How often do you have someone to love and make you feel wanted?
#' 40192480: How often do you have someone to take you to the doctor if you need it?
#' 40192511: How often do you have someone to help you with daily chores if you were sick?
#' 40192528: How often do you have someone to turn to for suggestions about how to deal with a personal problem?
#' Ref: https://www.rand.org/health-care/surveys_tools/mos/social-support/survey-instrument.html
#' @export
sdoh_survey_social_support <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	social_support_dat <- aou.reader::survey_query(c(40192442, 40192480, 40192388, 40192511, 
                          							 40192439, 40192528, 40192399, 40192446))
	social_support_dat[, item_score := fcase(survey_response == "None of the time", 1,
											 survey_response == "A little of the time", 2,
											 survey_response == "Some of the time", 3,
											 survey_response == "Most of the time", 4,
											 survey_response == "All of the time", 5,
											 default = NA)]
	social_support_agg <- social_support_dat[, sdoh_survey_social_support_score := mean(item_score, na.rm = T), .(person_id)]
	.write_to_bucket(social_support_agg, output_folder, "sdoh_survey_social_support")
}