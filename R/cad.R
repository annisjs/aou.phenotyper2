#' CAD
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/cad.csv
#' @details At least 2 ICD codes and 1 CPT code.
#'
#' ICD9: "410","410.%","411","411.%","412","412.%","413","413.%","414","414.%","V45.82"
#'
#' ICD10: "I25.1%"
#'
#' CPT: "33534","33535","33536","33510","33511","
#' 33512","33513","33514","33515","33516",
#' "33517","33518","33519","33520","33521","33522",
#' "33523","92980","92981","92982","92984","92995","92996"
#' @export
cad <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes = c("410","410.%","411","411.%","412","412.%","413","413.%","414","414.%","V45.82")
    icd10_codes = c("I25.1%")
    cpt_codes = c("33534","33535","33536","33510","33511","33512","33513","33514","33515","33516","33517","33518","33519","33520","33521","33522","33523","92980","92981","92982","92984","92995","92996")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_cpt <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
    result_icd <- rbind(result_icd9,result_icd10)
    result_icd_agg <- result_icd[,.(icd_status = length(condition_start_date) > 1,
                                    icd_entry_date = min(condition_start_date)),.(person_id)]
    result_cpt_agg <- result_cpt[,.(cpt_status = length(entry_date) > 0,
                                    cpt_entry_date = min(entry_date)),.(person_id)]
    result_all <- merge(result_icd_agg,result_cpt_agg,by="person_id",all.x=TRUE,all.y=TRUE)
    result_all[is.na(cpt_status),cpt_status := FALSE]
    result_all[is.na(icd_status),icd_status := FALSE]
    result_all <- result_all[,.(cad_entry_date = min(c(icd_entry_date,cpt_entry_date),na.rm=T),
                                cad_status = cpt_status | icd_status),.(person_id)]
    result_all[cad_status == FALSE, cad_entry_date := NA]
    .write_to_bucket(result_all,output_folder,"cad")
}
