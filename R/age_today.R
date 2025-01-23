#' Age Today
#'
#' @param output_folder the folder to write the output
#' @return output_folder/age_today.csv
#' @import data.table
#' @export
age_today <- function(output_folder)
{
    agecalc <- function(dob){
      require(data.table)
      current_date <- Sys.Date()
      age <- year(current_date) - year(dob) - 1
      ii <- (month(current_date) > month(dob)) | (month(current_date) == month(dob) & 
                                                    mday(current_date) >= mday(dob))
      age[ii] <- age[ii] + 1
      return(age)
    }

    dems <- aou.reader::demographics_query() 
    dems_dt <- as.data.table(dems)
    dems_dt[, age_today := agecalc(date_of_birth)]

    result_dt <- dems_dt[, c("person_id","age_today")]


    .write_to_bucket(result_dt,output_folder,"age_today")
}
