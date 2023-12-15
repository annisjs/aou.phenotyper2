#' Rent/own
#' @export
#' @return output_folder/rent_own.csv
#' @import data.table stringr aou.reader
rent_own <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query("1585370")
  colnames(result) <- c("person_id","rent_own_status","rent_own_entry_date")
  .write_to_bucket(result,output_folder,"rent_own")
}

