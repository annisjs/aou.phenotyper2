#' Overall Health Survey: PROMIS Physical Health
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
# PROMIS Physical Health
#'      This first scale is a two-item measure of one's personal rating of their physical health, and how well a participant is able to carry out everyday activities.
#'
#'      This scale is comprised of 2 question concept IDs, listed below. To get only the information relevant to PROMIS Phycial Health,
#'      we can filter the dataset to include only those question concept IDs
#'
#'     - **1585723**: How would you rate your physical health?
#'     - **1585741**: To what extent are you able to carry out your everyday physical activities?
#'
#'     Ref: http://www.healthmeasures.net/images/PROMIS/manuals/PROMIS_Global_Scoring_Manual.pdf
#'
#'   For this scale, we will rate physical health on a scale of 1-5, with higher scores corresponding to better physical health.
#'
#'   For general physical health, we rate as follows:
#'   - 1: Poor
#'   - 2: Fair
#'   - 3: Good
#'   - 4: Very Good
#'   - 5: Excellent
#'
#'   For everyday activities, we rate as follows:
#'   - 1: Not at All
#'   - 2: A Little
#'   - 3: Moderately
#'   - 4: Mostly
#'   - 5: Completely
#'
#' The summed score is then coverted to a t-score according to the PROMIS scoring manual
#' @return output_folder/overal_health_survey_promis_phys.csv
#' @export
overall_health_survey_promis_phys <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	result <- aou.reader::survey_query(c("1585723", "1585741"))
	result[, item_score := data.table::fcase(survey_response %in% c("General Physical Health: Poor", "Everyday Activities: Not At All"), 1,
                                 survey_response %in% c("General Physical Health: Fair", "Everyday Activities: A Little"), 2,
                                 survey_response %in% c("General Physical Health: Good", "Everyday Activities: Moderately"), 3,
                                 survey_response %in% c("General Physical Health: Very Good", "Everyday Activities: Mostly"), 4,
                                 survey_response %in% c("General Physical Health: Excellent", "Everyday Activities: Completely"), 5)]
    result_agg <- result[, .(raw_score = sum(item_score),
                             n_responses = length(which(!is.na(item_score))),
                             survey_date = survey_date[1]),
                         .(person_id)]
    physical_lookup <- data.table(
        raw_score = 2:10,
        physical_tscore = c(
            23.4, 29.0, 33.4, 37.3, 41.1,
            45.0, 50.0, 56.0, 63.3
        )
    )
    result_agg <- merge(result_agg, physical_lookup, by = "raw_score")
    result_agg <- result_agg[n_responses == 2]
	result_agg <- result_agg[, .(overall_health_survey_promis_phys_score = physical_tscore[1],
                                 overall_health_survey_promis_phys_date = survey_date[1]),
                               .(person_id)]
    result_agg <- result_agg[, c("person_id", 
                                 "overall_health_survey_promis_phys_date",
                                 "overall_health_survey_promis_phys_score")]
	.write_to_bucket(result_agg, output_folder, "overall_health_survey_promis_phys")
}
