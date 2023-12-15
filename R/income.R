#' Income
#' @export
#' @return output_folder/income.csv
#' @import data.table stringr aou.reader
income <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1585375",anchor_date_table,before,after)
    colnames(result) <- c("person_id","income_value","income_entry_date")
    .write_to_bucket(result,output_folder,"income")
}

