#' SDOH Survey: Discrimination in medical settings
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_discrimination_medical.csv
#' @details 7 items evaluate discrimination specifically in healthcare settings:
#' 
#' 40192383: How often does a doctor or nurse act as if he or she is better than you?
#' 40192394: How often do you feel like a doctor or nurse is not listening to what you were saying?
#' 40192423: How often does a doctor or nurse act as if he or she is afraid of you?
#' 40192425: How often are you treated with less respect than other people?
#' 40192497: How often are you treated with less courtesy than other people?
#' 40192505: How often does a doctor or nurse act as if he or she thinks you are not smart?
#' @export
sdoh_survey_discrimination_medical <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	result <- aou.reader::survey_query(c(40192497, 40192425, 40192425, 40192505, 40192423, 40192383, 40192394))
	result[, item_score := fcase(survey_response == "Always", 5,
                                 survey_response == "Most of the time", 4,
                                 survey_response == "Sometimes", 3,
                                 survey_response == "Rarely", 2,
                                 survey_response == "Never", 1,
                                 default = NA)]
	result_agg <- result[, .(sdoh_survey_discrimination_medical_score = mean(item_score, na.rm = T)), .(person_id)]
	.write_to_bucket(result_agg, output_folder, "sdoh_survey_discrimination_medical")
}