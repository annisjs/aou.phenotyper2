#' Employment
#' @export
#' @return output_folder/employment.csv
#' @import data.table stringr aou.reader
employment <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1585952",anchor_date_table,before,after)
    colnames(result) <- c("person_id","employment_status","employment_date")
    .write_to_bucket(result,output_folder,"employment")
}

