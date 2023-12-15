#' Bout Cadence
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/bout_cadence.csv
#' @details
#' Bout cadence is the average steps per minute when the step count >/= 60 steps a minute for at least 2 minutes.
#' The wearer will usually have many of these “bouts” throughout the day.
#' We take the average over the entire day to get the average bout cadence.
#' @import stringr bigrquery data.table
#' @export
bout_cadence <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  bucket <- Sys.getenv("WORKSPACE_BUCKET")
  output_folder <- file.path(bucket,output_folder)
  dataset <- Sys.getenv("WORKSPACE_CDR")
  query <- str_glue("
        SELECT person_id,
               CAST(datetime AS DATE) as bout_cadence_date,
               AVG(steps) as bout_cadence_value
        FROM (SELECT `{dataset}.steps_intraday`.*,
                     lag (datetime) over (partition by person_id, CAST(datetime AS DATE) order by datetime) as nextTimestamp_lag,
                     lead (datetime) over (partition by person_id, CAST(datetime AS DATE) order by datetime) as nextTimestamp_lead
              from `{dataset}.steps_intraday`
              where steps >= 60
             ) t
        WHERE
          (DATE_DIFF(datetime,nextTimestamp_lag,minute) <= 1 OR
          DATE_DIFF(nextTimestamp_lead,datetime,minute) <= 1)
        GROUP BY
        CAST(datetime AS DATE),person_id
      ")
  bq_table_save(
    bq_dataset_query( query, billing = Sys.getenv("GOOGLE_PROJECT")),
    paste0(output_folder,"/aou_phenotyper/bout_cadence_*.csv"),
    destination_format = "CSV")
  result <- read_bucket(paste0(output_folder,"/aou_phenotyper/bout_cadence_*.csv"))
  if (!is.null(anchor_date_table))
  {
    result <- as.data.table(merge(result,anchor_date_table,by="person_id"))
    result[,min_window_date := anchor_date + before]
    result[,max_window_date := anchor_date + after]
    result <- result[bout_cadence_date >= min_window_date]
    result <- result[bout_cadence_date <= max_window_date]
    result <- result[,c("person_id","bout_cadence_date","bout_cadence_value")]
  }
  fwrite(result,file="bout_cadence.csv")
  system(str_glue("gsutil cp bout_cadence.csv {output_folder}/bout_cadence.csv"),intern=TRUE)
  system(str_glue("gsutil rm {output_folder}/aou_phenotyper/*"),intern=TRUE)
}
