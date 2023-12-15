#' Health insurance
#' @return output_folder/health_insurance.csv
#' @export
health_insurance <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1585386",anchor_date_table,before,after)
    colnames(result) <- c("person_id","health_insurance_status","health_insurance_entry_date")
    .write_to_bucket(result,output_folder,"health_insurance")
}

