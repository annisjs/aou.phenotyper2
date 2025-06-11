#' DCM OUTCOMES icd_prevalent
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm_outcomes_icd_prevalent.csv
#' @details At least 1 ICD code:
#' @export
dcm_outcomes_icd_prevalent <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c('00.51','00.54','37.94','37.95','37.96','37.97','37.98','89.49','V45.02','996.04','V53.32')
    icd10_codes <- c('Z45.02')
    icd_proc_codes <- c('02H40KZ','02H43KZ','02H44KZ','02H60KZ','02H63KZ','02H64KZ','02H70KZ','02H73KZ','02H74KZ',
                          '02HK0KZ','02HK3KZ','02HK4KZ','02HL0KZ','02HL3KZ','02HL4KZ','02HN0KZ','02HN3KZ','02HN4KZ',
                          '0JH608Z','0JH609Z','0JH638Z','0JH639Z','0JH808Z','0JH809Z','0JH838Z','0JH839Z')
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_icd_proc <- aou.reader::icd_procedure_query(icd_proc_codes,anchor_date_table,before,after)
    #make proc output date match icd output date name
    setnames(result_icd_proc, "icd_procedure_date", "condition_start_date")
    setnames(result_icd_proc, "icd_procedure_code", "condition_source_value")
    result_all <- rbind(result_icd9,result_icd_proc)
    result_all <- rbind(result_all,result_icd10)
    cpt <- c('33216','33217','33218','33219','33220','33223','33224','33225','33230','33231','33240','33241',
                     '33243','33244','33249','33262','33263','33264','33270','33271','33272','33273','93260','93261',
                     '93282','93283','93284','4470F','93640','93641','93642','93644')

    cpt_dat <- aou.reader::cpt_query(cpt,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    final <- rbind(result_all,cpt_dat)
    final <- final[,.(dcm_outcomes_icd_prevalent_status = length(condition_start_date) > 0,
                                dcm_outcomes_icd_prevalent_entry_date = min(condition_start_date)),
                                .(person_id)]
    .write_to_bucket(final,output_folder,"dcm_outcomes_icd_prevalent")
}