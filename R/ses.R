#' Socioeconomic status variables
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. The dataset will be merged with demographics.
#' @param before serves no function
#' @param after serves no function
#' @return output_folder/ses.csv
#' @export
ses <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::ses_query(anchor_date_table,before,after)
  colnames(result) <- c("person_id","ses_observation_datetime","ses_zip_code","ses_assisted_income",
                        "ses_high_school_education","ses_median_income","ses_no_health_insurance","ses_poverty",
                        "ses_vacant_housing","deprivation_index","american_community_survey_year",
                        "state_of_residence")
  .write_to_bucket(result,output_folder,"ses")
}
