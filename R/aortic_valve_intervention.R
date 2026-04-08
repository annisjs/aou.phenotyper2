#' aortic valve intervention Definition
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hcm.csv
#' @export
aortic_valve_intervention <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c("35.05","35.21","35.22")

    icd10_codes <- c("02RF3JZ","02RF4J","02RF07Z","02RF08Z","02RF0KZ","02RF0JZ")

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    cpt_codes <- c("33361","33362","33363","33364","33365","33366","33367","33368","33369",
                     "33405","33406","33410","33411","33412","33413","92986")


    cpt_dat <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    combined <- rbind(result_all,cpt_dat)

    final_write <- combined[,.(aortic_valve_intervention_date = min(condition_start_date),
                                    aortic_valve_intervention_status = length(condition_start_date) >= 1),
                                .(person_id)]

  
    final <- filtered_write[, c("person_id", "aortic_valve_intervention_status","aortic_valve_intervention_date")]

    .write_to_bucket(final,output_folder,"aortic_valve_intervention")
}