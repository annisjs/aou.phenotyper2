#' hcm Definition
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hcm.csv
#' @export
hcm_2 <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    hcm_ob_icd9_codes <- c('425.18')
    hcm_ob_icd10_codes <- c('I42.1')

    hcm_ob_result_icd9 <- aou.reader::icd9_query(hcm_ob_icd9_codes,anchor_date_table,before,after)
    hcm_ob_result_icd10 <- aou.reader::icd10_query(hcm_ob_icd10_codes,anchor_date_table,before,after)
    hcm_ob_result_all <- rbind(hcm_ob_result_icd9,hcm_ob_result_icd10)

    hcm_other_icd9_codes <- c('425.11')
    hcm_other_icd10_codes <- c('I42.2')

    hcm_other_result_icd9 <- aou.reader::icd9_query(hcm_other_icd9_codes,anchor_date_table,before,after)
    hcm_other_result_icd10 <- aou.reader::icd10_query(hcm_other_icd10_codes,anchor_date_table,before,after)
    hcm_other_result_all <- rbind(hcm_other_result_icd9,hcm_other_result_icd10)

    #need at least 2 codes on seperate days

    #count distinct dates for each subject
    distinct_counts <- aggregate(condition_start_date ~ person_id, data = hcm_other_result_all,
                            FUN = function(x) length(unique(x)))

    #keep only combinations with 2+ distinct dates
    valid_ids <- distinct_counts[distinct_counts$condition_start_date >= 2, "person_id"]

    #filter original data frame
    hcm_other_result_final <- hcm_other_result_all[hcm_other_result_all$person_id %in% valid_ids, ]

    final <- rbind(hcm_other_result_final,hcm_ob_result_all)



    final_write <- final[,.(hcm_2_entry_date = min(condition_start_date),
                                    hcm_2_status = length(condition_start_date) >= 1),
                                .(person_id)]

    #final_post_age <- final_write[, c("person_id", "hcm_status","hcm_entry_date")]






    #final <- combined %>%
     #   group_by(person_id) %>%
     #   filter(n_distinct(condition_start_date) >= 2)




    .write_to_bucket(final_write,output_folder,"hcm_2")
}