#' SDOH Survey: Discrimination
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_discrimination.csv
#' @details The next 9 items summarize survey participants' experiences with discrimination in everyday life:
#'
#' 40192380: How often do people act as if they are afraid of you?
#' 40192395: How often do people act as if they think you are dishonest?
#' 40192416: How often do you receive poorer service than other people at restaurants or stores?
#' 40192451: How often are you threatened or harassed?
#' 40192466: How often are you treated with less courtesy than other people?
#' 40192489: How often are you treated with less respect than other people?
#' 40192490: How often do people act as if they think you are not smart?
#' 40192496: How often do people act as if they're better than you are?
#' 40192519: How often are you called names or insulted?
#' 
#' Refs:
#' Williams, D.R., Yu, Y., Jackson, J.S., and Anderson, N.B. “Racial Differences in Physical and Mental Health: Socioeconomic Status, Stress, and Discrimination.” Journal of Health Psychology. 1997; 2(3):335-351.
#' Taylor T.R., Kamarck T.W., Shiffman S. “Validation of the Detroit area study discrimination scale in a community sample of older African American adults: the Pittsburgh healthy heart project.” International Journal of Behavioral Medicine. 2004; 11:88–94.
#' @export
sdoh_survey_discrimination <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	result <- aou.reader::survey_query(c(40192466, 40192489, 40192416, 40192490, 
                                         0192380, 40192395, 40192496, 40192519, 40192451))
	result[, item_score := fcase(survey_response == "Almost everyday", 5,
                                 survey_response == "At least once a week", 4,
                                 survey_response == "A few times a month", 3,
                                 survey_response == "A few times a year", 2,
                                 survey_response == "Less than once a year", 1,
                                 survey_response == "Never", 0,
                                 default = NA)]
	result_agg <- result[, .(sdoh_survey_discrimination_score = mean(item_score, na.rm = T)), .(person_id)]
	.write_to_bucket(result_agg, output_folder, "sdoh_survey_discrimination")
}