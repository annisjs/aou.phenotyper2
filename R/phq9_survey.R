#' PHQ-9 Survey
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/phq9_survey.csv
#' @export
phq9_survey <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    codes <- c(1704041, 
               1704039, 
               1703983, 
               1703977,
               1704038, 
               1704004, 
               1703996, 
               1704024, 
               1704026
    )
    phq9 <- aou.reader::survey_obs_query(codes, anchor_date_table, before, after)
    phq9[, survey_response_recode := 
        fcase(
            survey_obs_response == "Not at all", 0L,
            survey_obs_response == "Several days", 1L,
            survey_obs_response == 'More than half the days', 2L,
            survey_obs_response == "Nearly every day", 3L,
            default = NA_integer_
        )
    ]
    # Avoids duplicate responses
    phq9_sub <- phq9[, 
        .(survey_response_recode = survey_response_recode[1]), 
        .(person_id, observation_date, survey_obs_question)
    ]
    phq9_sub[,
        n_responses := length(which(!is.na(survey_response_recode))), 
        .(person_id)
    ]
    phq9_sub <- phq9_sub[n_responses == 9]
    phq9_agg <- phq9_sub[, 
         .(phq9_total = sum(survey_response_recode)),
         .(person_id, observation_date)
    ]
    data.table::setnames(phq9_agg, "observation_date", "phq9_date")
    .write_to_bucket(phq9_agg, output_folder, "phq9_survey")
}
