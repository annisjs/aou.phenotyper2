#' Stable house concern
#' @export
#' @return output_folder/stable_house_concern.csv
#' @import data.table stringr aou.reader
#' @description In the past 6 months, have you been worried or concerned about NOT having a place to live?
stable_house_concern <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query("1585886",anchor_date_table,before,after)
  colnames(result) <- c("person_id","stable_house_concern_status","stable_house_concern_entry_date")
  .write_to_bucket(result,output_folder,"stable_house_concern_status")
}

