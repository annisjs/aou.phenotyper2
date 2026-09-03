#' SDOH Survey: Neighborhood disorder
#'
#' @param output_folder The folder to write the output.
#' @param anchor_date_table A data.frame containing two columns:
#'   person_id and anchor_date. A time window can be defined around the anchor
#'   date using the \code{before} and \code{after} arguments.
#' @param before An integer greater than or equal to 0. Dates prior to
#'   anchor_date - before will be excluded.
#' @param after An integer greater than or equal to 0. Dates after
#'   anchor_date + after will be excluded.
#'
#' @return output_folder/sdoh_survey_neighborhood_disorder.csv
#'
#' @details The following 13 questions measure perceived physical and social
#'   disorder in a participant's neighborhood:
#'
#' 40192420: There is a lot of graffiti in your neighborhood
#' 40192522: Your neighborhood is noisy
#' 40192412: Vandalism is common in your neighborhood
#' 40192469: There are a lot of abandoned buildings in your neighborhood
#' 40192456: Your neighborhood is clean
#' 40192386: People in your neighborhood take good care of their houses and apartments
#' 40192500: There are too many people hanging around on the streets near your home
#' 40192493: There is a lot of crime in your neighborhood
#' 40192457: There is too much drug use in your neighborhood
#' 40192476: There is too much alcohol use in your neighborhood
#' 40192404: You are always having trouble with your neighbors
#' 40192400: People in your neighborhood watch out for each other
#' 40192384: Your neighborhood is safe
#'
#' The four positively worded items concerning cleanliness, property care,
#' neighbors watching out for one another, and neighborhood safety are
#' reverse-coded. The final score is the mean of all 13 items and ranges from
#' 1 to 4. Higher scores indicate greater perceived neighborhood disorder.
#' Participants must have valid responses to all 13 items to receive a score.
#'
#' @export
sdoh_survey_neighborhood_disorder <- function(
    output_folder,
    anchor_date_table = NULL,
    before = NULL,
    after = NULL
) {
    result <- aou.reader::survey_query(
        c(
            40192420,
            40192522,
            40192412,
            40192469,
            40192500,
            40192493,
            40192457,
            40192476,
            40192404
        ),
        anchor_date_table,
        before,
        after
    )

    result_reverse <- aou.reader::survey_query(
        c(
            40192456,
            40192386,
            40192400,
            40192384
        ),
        anchor_date_table,
        before,
        after
    )

    result[
        ,
        item_score := fcase(
            survey_response == "Strongly agree", 4,
            survey_response == "Agree", 3,
            survey_response == "Disagree", 2,
            survey_response == "Strongly disagree", 1,
            default = NA_real_
        )
    ]

    result_reverse[
        ,
        item_score := fcase(
            survey_response == "Strongly agree", 1,
            survey_response == "Agree", 2,
            survey_response == "Disagree", 3,
            survey_response == "Strongly disagree", 4,
            default = NA_real_
        )
    ]

    result <- rbind(
        result,
        result_reverse
    )

    result_agg <- result[
        ,
        .(
            sdoh_survey_neighborhood_disorder_score =
                round(mean(item_score, na.rm = TRUE), 2),

            sdoh_survey_neighborhood_disorder_date =
                survey_date[1],

            n_responses =
                length(which(!is.na(item_score)))
        ),
        .(person_id)
    ]

    result_agg <- result_agg[
        n_responses == 13
    ]

    result_agg[
        ,
        n_responses := NULL
    ]

    .write_to_bucket(
        result_agg,
        output_folder,
        "sdoh_survey_neighborhood_disorder"
    )
}
