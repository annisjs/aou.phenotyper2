#' Sexual orientation
#' 
#' @return output_folder/sexual_orientation.csv
#' @export
sexual_orientation <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1585899",anchor_date_table,before,after)
    colnames(result) <- c("person_id","sexual_orientation_status","sexual_orientation_entry_date")
    .write_to_bucket(result,output_folder,"sexual_orientation")
}

