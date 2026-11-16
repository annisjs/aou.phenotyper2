#' GAD-7 Survey
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/gad7_survey.csv
#' @export
gad7_survey <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    codes <- c(1704000, 1703920, 1704042, 1703995, 1703987, 1703984, 1704028)
    gad7 <- aou.reader::survey_obs_query(codes, anchor_date_table, before, after)
    gad7[, survey_response_recode := 
        fcase(
            survey_obs_response == "Not at all", 0L,
            survey_obs_response == "Several days", 1L,
            survey_obs_response == 'More than half the days', 2L,
            survey_obs_response == "Nearly every day", 3L,
            default = NA_integer_
        )
    ]
    # Avoids duplicate responses
    gad7_sub <- gad7[, 
        .(survey_response_recode = survey_response_recode[1]), 
        .(person_id, observation_date, survey_obs_question)
    ]
    gad7_sub[,
        n_responses := length(which(!is.na(survey_response_recode))), 
        .(person_id)
    ]
    gad7_sub <- gad7_sub[n_responses == 7]
    gad7_agg <- gad7_sub[, 
         .(gad7_total = sum(survey_response_recode)),
         .(person_id, observation_date)
    ]
    data.table::setnames(gad7_agg, "observation_date", "gad7_date")
    .write_to_bucket(gad7_agg, output_folder, "gad7_survey")
}
