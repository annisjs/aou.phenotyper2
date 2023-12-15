#' Obesity using BMI (>/= 30)
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/obesity_bmi.csv
#' @details Finds first date where BMI >/= 30
#' @export
obesity_bmi <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result_bmi <- aou.reader::bmi_query(anchor_date_table,before,after)
  result_height <- aou.reader::height_query(anchor_date_table,before,after)
  result_weight <- aou.reader::weight_query(anchor_date_table,before,after)
  result_all <- merge(result_weight,result_height,by=c("person_id","measurement_date"))
  result_all[, bmi := weight / (height/100)^2]
  result_all <- result_all[,c("person_id","measurement_date","bmi")]
  result_all <- rbind(result_all,result_bmi)
  result_all <- result_all[order(measurement_date)]
  result_all <- result_all[bmi >= 30]
  result_all <- result_all[,.(obesity_bmi_entry_date = measurement_date[1],
                              obesity_bmi_value = bmi[1]),
                           .(person_id)]
  .write_to_bucket(result_all,output_folder,"obesity_bmi")
}
