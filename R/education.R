#' Education
#' 
#' @return output_folder/education.csv
#' @export
education <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1585940",anchor_date_table,before,after)
    colnames(result) <- c("person_id","education_status","education_entry_date")
    .write_to_bucket(result,output_folder,"education")
}

