#' hfpef
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hfpef.csv
#' @details At least 1 ICD code
#'
#' ICD9: "428.32"
#'
#' ICD10: "I50.32"
#' @export
hfpef <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    #print(getwd())
    #call heart failure algo
    #source("heart_failure.R")
    #heart_failure(output_folder,anchor_date_table,before,after) #this writes to bucket as it's an algo not exactly designed for this
    #hf_data <- .read_from_bucket(output_folder,"heart_failure") #read data from bucket
    loc <- str_glue("{my_bucket}/datasets/hermes_heart_failure.csv") 
    hf_data_all <- read_bucket(loc) #read data from bucket
    hf_data <- hf_data_all[hf_data_all$all_hf_pheno_1_case == TRUE, ] #keep only those with hf as this algo returns all grids and flags hf/control

    #head(hf_data)

    #an.error.occured <- NULL
    #tryCatch( { result <- .read_from_bucket(output_folder,"heart_failure"); print(res) }
    #      , error = function(e) {an.error.occured <<- "Please run the heart_failure algorithm before the hfpef algorithm."})
    #print(an.error.occured)

    #hf_data <- result
    #print(head(hf_data))

    #EF
    result_ef <- aou.reader::ef_query(anchor_date_table,before,after)
    colnames(result_ef) <- c("person_id","all_ef_entry_date","all_ef_value")
    #filter for HFPEF
    #need to check if ef is at or over 50, but also has never been below 50
    under_50 <- result_ef[result_ef$all_ef_value < 50, ] #ef has ever been below 50?
    over_or_at_50 <- result_ef[result_ef$all_ef_value >= 50, ]
    pef_by_ef <- over_or_at_50[!over_or_at_50$person_id %in% under_50$person_id, ] #ef is above or at 50 and has never been below 50

    #switch this to inner join?
    hfpef_ef_code <- pef_by_ef[pef_by_ef$person_id %in% hf_data$person_id] #pef, but also in the hf cohort aka hfpef
    colnames(hfpef_ef_code) <- c("person_id","hfpef_entry_date","hfpef_value")

    icd9_codes <- c("428.3","428.30","428.31","428.32","428.33","428.34","428.35","428.36","428.37","428.38","428.39")
    icd10_codes <- c("I50.3","I50.30","I50.31","I50.32","I50.33","I50.34","I50.35","I50.36","I50.37","I50.38","I50.39")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_hfpef_codes <- rbind(result_icd9,result_icd10)
    colnames(result_hfpef_codes) <- c("person_id","hfpef_entry_date","hfpef_value")

    #combine all
    result_all <- rbind(result_hfpef_codes,hfpef_ef_code)
    result_all$first_entry <- with(result_all, ave(hfpef_entry_date, person_id, FUN = min)) #group by person_id & get first entry date per group

    result_all <- result_all[,.(hfpef_entry_date = first_entry,
                                hfpef_status = length(first_entry) > 0),
                .(person_id)]
    .write_to_bucket(result_all,output_folder,"hfpef")
}
