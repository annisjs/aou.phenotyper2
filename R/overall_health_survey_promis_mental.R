#' Overall Health Survey: PROMIS Mental Health
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' PROMIS Mental Health
# PROMIS Mental Health
#'
#'   The next two-item scale measures a person's self-reported mental health by evaluating their general mental wellbeing and their social satisfaction:
#'
#'   - **1585729**: How would you rate your mental health?
#'   - **1585735**: How would you rate your satisfaction with your social activities and relationships?
#'
#'   Ref: http://www.healthmeasures.net/images/PROMIS/manuals/PROMIS_Global_Scoring_Manual.pdf
#'
#' @return output_folder/overal_health_survey_promis_mental.csv
#' @export
overall_health_survey_promis_mental <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	result <- aou.reader::survey_query(c("1585729", "1585735"))
	result[, item_score := data.table::fcase(
                    survey_response %in% c("General Mental Health: Poor", "Social Satisfaction: Poor"), 1,
                    survey_response %in% c("General Mental Health: Fair", "Social Satisfaction: Fair"), 2,
                    survey_response %in% c("General Mental Health: Good", "Social Satisfaction: Good"), 3,
                    survey_response %in% c("General Mental Health: Very Good", "Social Satisfaction: Very Good"), 4,
                    survey_response %in% c("General Mental Health: Excellent", "Social Satisfaction: Excellent"), 5)]
    result_agg <- result[, .(raw_score = sum(item_score),
                             n_responses = length(which(!is.na(item_score))),
                             survey_date = survey_date[1]),
                         .(person_id)]
    lookup <- data.table(
        raw_score = 2:10,
        tscore = c(
                25.8, 32.0, 36.5, 40.6, 44.4,
                48.6, 52.8, 57.7, 64.6
        )
    )
    result_agg <- merge(result_agg, lookup, by = "raw_score")
    result_agg <- result_agg[n_responses == 2]
	result_agg <- result_agg[, .(overall_health_survey_promis_mental_score = tscore[1],
                                 overall_health_survey_promis_mental_date = survey_date[1]),
                               .(person_id)]
    result_agg <- result_agg[, c("person_id", 
                                 "overall_health_survey_promis_mental_date",
                                 "overall_health_survey_promis_mental_score")]
	.write_to_bucket(result_agg, output_folder, "overall_health_survey_promis_mental")
}
