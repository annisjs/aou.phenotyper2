#' Top 10 Steps
#' @export
#' @return output_folder/top10_steps.csv
#' @import stringr bigrquery
top10_steps <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::top_steps_query(10,anchor_date_table,before,after)
  .write_to_bucket(result,output_folder,"top10_steps",TRUE,"top_steps_query.csv")
}
