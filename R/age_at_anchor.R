#' Age At Anchor
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @export
#' @return output_folder/age_at_anchor.csv
#' @details gives age at the passed in anchor date
age_at_anchor <-  function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    if (is.null(anchor_date_table))
    {
    stop("age_at_anchor is not a primary variable and requires an anchor date table.")
    }

    agecalc <- function(dob,anchor_date){
      require(data.table)
      age <- year(anchor_date) - year(dob) - 1
      ii <- (month(anchor_date) > month(dob)) | (month(anchor_date) == month(dob) & 
                                                    mday(anchor_date) >= mday(dob))
      age[ii] <- age[ii] + 1
      return(age)
    }

    dems <- aou.reader::demographics_query() 
    dems_dt <- as.data.table(merge(anchor_date_table,dems,by="person_id",all.x = TRUE))

    dems_dt[, age_at_anchor := agecalc(as.Date(date_of_birth),as.Date(anchor_date))]

    result_dt <- dems_dt[, c("person_id","age_at_anchor")]


    .write_to_bucket(result_dt,output_folder,"age_at_anchor")
}
