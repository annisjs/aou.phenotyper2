#' Cadence patterns from step counts
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/cadence_pattern.csv
#' @details Non-movement (minutes/day) = 0 steps/min but has heart rate data for that minute
#' Incidental movement (minutes/day) = 1-19 steps/min *
#' Sporadic movement(minutes/day) = 20-39 steps/min
#' Purposeful movement (minutes/day) = 40-59 steps/min
#' Slow walking (minutes/day) = 60-79 steps/min
#' Medium walking (minutes/day) = 80-99 steps/min
#' Brisk walking (minutes/day) = 100-119 steps/min
#' All faster ambulation (minutes/day) = 120+ steps/min
#' @export
cadence_pattern <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::cadence_pattern(anchor_date_table,before,after)
  .write_to_bucket(result,output_folder,"cadence_pattern")
}
