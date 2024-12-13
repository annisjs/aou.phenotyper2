#' Resting heart rate
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/resting_heart_rate.csv
#' @details Looks for HR during sleep, and then takes minimum value of that set of observed values.
#' @export
resting_heart_rate <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL,cohort=NULL)
{
  first_asleep_dat <- first_asleep(anchor_date_table=anchor_date_table, before=before, after=after)
  last_asleep_dat <- last_asleep(anchor_date_table=anchor_date_table, before=before, after=after)
  hourly_min_hr_dat <- hourly_min_heart_rate(anchor_date_table=anchor_date_table, before=before, after=after, cohort=cohort)
  last_asleep_dat[, last_asleep_date_copy := as.Date(last_asleep_date)]
  sleep_dat <- merge(first_asleep_dat, 
            last_asleep_dat, 
            by.x = c("person_id", "first_asleep_date"),
            by.y = c("person_id", "last_asleep_date")) 
  rm("last_asleep_dat", "first_asleep_dat")
  sleep_dat[, last_asleep_datetime := last_asleep_date_copy + lubridate::seconds(60*last_asleep_duration)]
  sleep_dat <- sleep_dat[first_asleep_datetime < last_asleep_datetime]
  hourly_min_hr_dat[, datetime := paste(date, hour)]
  hourly_min_hr_dat[, datetime := as.POSIXct(datetime, format = "%Y-%m-%d %H")]
  hourly_min_hr_dat[, datetime2 := datetime]
  data.table::setkey(hourly_min_hr_dat, person_id, datetime, datetime2)
  data.table::setkey(sleep_dat, person_id, first_asleep_datetime, last_asleep_datetime)
  idx <- data.table::foverlaps(hourly_min_hr_dat, sleep_dat, type = "within", nomatch = NULL, which = TRUE)
  resting_hr <- hourly_min_hr_dat[idx$xid]
  resting_hr_agg <- resting_hr[, .(resting_heart_rate = min(min_heart_rate)), .(person_id, date)]
  .write_to_bucket(resting_hr_agg,output_folder,"resting_heart_rate")
}
