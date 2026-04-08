#' hcm hermes control Definition. DO NOT EDIT without permission from Brandon
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hcm_hermes_case.csv
#' @export
hcm_hermes_control <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c('425.11','425.18','425.1')

    icd10_codes <- c('I42.1','I42.2')

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    #get all subjects
    dems <- aou.reader::demographics_query()




    #calc age at a given date
    agecalc <- function(dob,anchor_date){
      require(data.table)
      if (is.null(anchor_date))
      {
        return(NULL)
      }
      age <- year(anchor_date) - year(dob) - 1
      ii <- (month(anchor_date) > month(dob)) | (month(anchor_date) == month(dob) & 
                                                    mday(anchor_date) >= mday(dob))
      age[ii] <- age[ii] + 1
      return(age)
    }

    #get dob
    dems <- aou.reader::demographics_query()
    last_medical_touch <- aou.reader::medical_encounter_query("last",anchor_date_table,before,after)

    #keep all subjects in dems that are NOT in result_all
    final <- dems[ !dems$person_id %in% result_all$person_id, ]

    #add in last medical touch
    final <- merge(final, last_medical_touch, by = "person_id")

    final[, age_at_last_touch := agecalc(date_of_birth,medical_encounter_entry_date)]

    filtered_final <- final[age_at_last_touch >= 15]

    final_2 <- filtered_final[,.(hcm_hermes_control_date = medical_encounter_entry_date,
                                hcm_hermes_control_status = TRUE),
                                .(person_id)]


    final_write <- final_2[, c("person_id", "hcm_hermes_control_status", "hcm_hermes_control_date")]

    .write_to_bucket(final_write,output_folder,"hcm_hermes_control")
}