#' SDOH Survey: Loneliness, ULS-8
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_loneliness.csv
#' @details The following 8 questions comprise the loneliness scale, which evaluates the social satisfaction of survey participants:
#' 40192390: How often do you feel that you are unhappy being so withdrawn?
#' 40192397: How often do you feel that there is no one you can turn to?
#' 40192398: How often do you feel left out?
#' 40192494: How often do you feel that people are around you but not with you?
#' 40192501: How often do you feel isolated from others?
#' 40192504: How often do you feel that you are an outgoing person?
#' 40192507: How often do you feel lack companionship?
#' 40192516: How often do you fell that you can find companionship when you want it?
#' 
#' Ref: Hays RD, DiMatteo MR. A short-form measure of loneliness. J Pers Assess. 1987 Spring;51(1):69-81. doi: 10.1207/s15327752jpa5101_6. PMID: 3572711
#' @export
sdoh_survey_loneliness <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  lonely_dat <- aou.reader::survey_query(c(40192507, 40192397, 40192398, 40192501, 40192390, 40192494))
  lonely_dat_rev <- aou.reader::survey_query(c(40192504, 40192516))
  lonely_dat[, item_score := fcase(survey_response == "Never", 1,
                                  survey_response == "Rarely", 2,
                                  survey_response == "Sometimes", 3,
                                  survey_response == "Often", 4,
                                  default = NA)]

  lonely_dat_rev[,  item_score := fcase(survey_response == "Never", 4,
                                        survey_response == "Rarely", 3,
                                        survey_response == "Sometimes", 2,
                                        survey_response == "Often", 1,
                                        default = NA)]

  lonely_dat <- rbind(lonely_dat, lonely_dat_rev)
  lonely_agg <- lonely_dat[, sdoh_survey_loneliness_score := mean(item_score, na.rm = T), .(person_id)]
  .write_to_bucket(lonely_agg, output_folder, "sdoh_survey_loneliness")
}