#' DCM OUTCOMES ppm_incident
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_ppm_incident.csv
#' @details At least 1 ICD code:
#' @export
dcm_outcomes_ppm_incident <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c('37.70','37.71','37.72','37.73','37.74','37.75','37.76','37.78','37.80','37.81','37.82','37.83')
    icd_proc_codes <- c('02HK0JZ','02HK0NZ','02HK3JZ','02HK3NZ','02HK4JZ','02HK4NZ','02HN0JZ',
                          '02HN3JZ','02HN4JZ','02H40NZ','02H43JZ','02H43NZ','02H44JZ','02H44NZ',
                          '02HL0JZ','02HL0NZ','02HL3JZ','02HL3NZ','02HL4JZ','02HL4NZ')
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd_proc <- aou.reader::icd_procedure_query(icd_proc_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd_proc)
    cpt <- c('0387T','33206','33207','33208','33210','33211','33214','33274','71090')

    cpt_dat <- aou.reader::cpt_query(cpt,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    final <- rbind(result_all,cpt_dat)
    final <- final[,.(dcm_outcomes_ppm_incident_status = length(condition_start_date) > 0,
                                dcm_outcomes_ppm_incident_entry_date = min(condition_start_date)),
                                .(person_id)]
    .write_to_bucket(final,output_folder,"dcm_outcomes_ppm_incident")
}