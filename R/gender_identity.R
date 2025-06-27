#' Gender identity
#' @return output_folder/gender_identity.csv
#' @export
gender_identity <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result <- aou.reader::survey_query("1585838",anchor_date_table,before,after)
    colnames(result) <- c("person_id","gender_identity_status","gender_identity_date")
    .write_to_bucket(result,output_folder,"gender_identity")
}
