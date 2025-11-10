#' SDOH Survey: Daily spiritual experiences
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_spiritual_exp.csv
#' @details This 6-item scale relates to the impact of religion and spirituality on the lives of survey participants:
#'
#' 40192401: How often do you feel deep inner peace or harmony?
#' 40192415: How often do you feel that you are spiritually touched by the beauty of creation?
#' 40192443: How often do you desire to be closer to or in union with God (or a higher power)?
#' 40192471: How often do you feel God's (or a higher power's) love for you, directly or through others?
#' 40192475: How often do you find strength and comfort in your religion?
#' 40192498: How often do you feel God's (or a higher power's) presence?
#' Ref: Masters K.S. (2013) Brief Multidimensional Measure of Religiousness/Spirituality (BMMRS). In: Gellman M.D., Turner J.R. (eds) Encyclopedia of Behavioral Medicine. Springer, New York, NY. https://doi.org/10.1007/978-1-4419-1005-9_1577
#' @export
sdoh_survey_spiritual_exp <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	result <- aou.reader::survey_query(c(40192498, 40192475, 40192401, 40192443, 40192471, 40192415))
	result[, item_score := fcase(survey_response == "Many times a day", 6,
                                 survey_response == "Every day", 5,
                                 survey_response == "Most days", 4,
                                 survey_response == "Some days", 3,
                                 survey_response == "Once in a while", 2,
                                 survey_response == "Never or almost never", 1,
                                 default = NA)]
	result_agg <- result[, sdoh_survey_spiritual_exp_score := mean(item_score, na.rm = T), .(person_id)]
	.write_to_bucket(result_agg, output_folder, "sdoh_survey_spirital_exp")
}