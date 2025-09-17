#' hcm Definition
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hcm.csv
#' @export
hcm <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c('425.11','425.18','425.1')

    icd10_codes <- c('I42.1','I42.2')

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    snomed_codes <- c('1204192000','704241002','871638006','1230303001','890119003','472326004',
                    '890122001','472325000','890121008','871649000','890120009','472316006','771509001',
                    '718713000','472318007','771513008','771478008','233871002','83978005','472324001',
                    '233873004','1204194004','471885006','195020003','45227007','700065003','63183009',
                    '719272007','389998005','445336009','53322007','93558006','41964001','389999002',
                    '15471000','195574002','443751000000108')


    snomed_dat <- aou.reader::snomed_query(snomed_codes,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    #setnames(cpt_dat, "entry_date", "condition_start_date")
    #setnames(cpt_dat, "cpt_code", "condition_source_value")

    combined <- rbind(result_all,snomed_dat)

    #need at least 2 codes on seperate days

    #count distinct dates for each subject
    distinct_counts <- aggregate(condition_start_date ~ person_id, data = combined,
                            FUN = function(x) length(unique(x)))

    #keep only combinations with 2+ distinct dates
    valid_ids <- distinct_counts[distinct_counts$condition_start_date >= 2, "person_id"]

    #filter original data frame
    final <- combined[combined$person_id %in% valid_ids, ]

    final_write <- final[,.(hcm_entry_date = min(condition_start_date),
                                    hcm_status = length(condition_start_date) >= 1),
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

    dems_dt[, age_at_anchor := agecalc(date_of_birth,hcm_entry_date)]

    filtered_dt <- dems_dt[age_at_anchor >= 15]

    final_post_age <- filtered_dt[, c("person_id", "hcm_status","hcm_entry_date")]






    #final <- combined %>%
     #   group_by(person_id) %>%
     #   filter(n_distinct(condition_start_date) >= 2)




    .write_to_bucket(final_post_age,output_folder,"hcm")
}