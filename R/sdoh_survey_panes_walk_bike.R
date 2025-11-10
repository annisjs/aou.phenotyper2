#' SDOH Survey: PANES - Walking and Bicycling
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_panes_walk_bike.csv
#' @details 
#' 40192410: My neighborhood has several free or low-cost recreation facilities
#' 40192431: There are facilities to bicycle in or near my neighborhood
#' 40192436: Many shops, stores, markets or other places to buy things I need are within easy walking distance of my home
#' 40192437: There are sidewalks on most of the streets in my neighborhood
#' 40192440: It is within a 10-15 minute walk to a transit stop
#' @export
sdoh_survey_panes_walk_bike <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query(c(40192410, 40192431, 40192436, 40192437, 40192440))
  result[, panes_item_score := fcase(survey_response == "Strongly agree", 4,
                                      survey_response == "Somewhat agree", 3,
                                      survey_response == "Somewhat disagree", 2,
                                      survey_response == "Strongly disagree", 1,
                                      default = NA)]
  result_agg <- result[, sdoh_survey_panes_walk_bike_score := mean(panes_item_score, na.rm = T), .(person_id)]
  .write_to_bucket(result_agg, output_folder, "sdoh_survey_panes_walk_bike")
}

