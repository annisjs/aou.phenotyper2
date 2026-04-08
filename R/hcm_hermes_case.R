#' hcm hermes case Definition. DO NOT EDIT without permission from Brandon
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hcm_hermes_case.csv
#' @export
hcm_hermes_case <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c('425.11','425.18','425.1')

    icd10_codes <- c('I42.1','I42.2')

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    #count distinct dates for each subject
    distinct_counts <- aggregate(condition_start_date ~ person_id, data = result_all,
                            FUN = function(x) length(unique(x)))

    #keep only combinations with 2+ distinct dates
    valid_ids <- distinct_counts[distinct_counts$condition_start_date > 1, "person_id"]

    #filter original data frame
    final <- result_all[result_all$person_id %in% valid_ids, ]

    final_write <- final[,.(hcm_hermes_case_date = min(condition_start_date),
                                    hcm_hermes_case_status = length(condition_start_date) >= 1),
                                .(person_id)]

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

    #add dob to df -- inner join
    dems_dt <- as.data.table(merge(final_write,dems,by="person_id"))

    dems_dt[, age_at_anchor := agecalc(date_of_birth,hcm_hermes_case_date)]

    filtered_dt <- dems_dt[age_at_anchor >= 15]

    final_post_age <- filtered_dt[, c("person_id", "hcm_hermes_case_status","hcm_hermes_case_date")]

    .write_to_bucket(final_post_age,output_folder,"hcm_hermes_case")
}