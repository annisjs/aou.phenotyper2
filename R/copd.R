#' COPD
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/copd.csv
#' @details Definition: Needs > 1 ICD code and 1 medication.
#'
#' ICD9: "491","491.0","491.1","491.2","491.20","491.21","491.22",
#' "491.8","491.9","492","492.0","492.8","496","496.0","493.21","493.22"
#'
#' ICD10: "J44.%","J43.%","J42","J41.%"
#'
#' Meds: "roflumilast","daliresp","tiotropium","spiriva","ipratropium","atrovent",
#' "theophylline","slo-bid","slo-phyllin","theo-dur","theo 24","theo24","theo-24",
#' "uniphyl","salmeterol","serevent","formoterol","foradil","albuterol + ipratropium",
#' "albuterol / ipratropium","ipratropium bromide / albuterol sulfate","ipratropium bromide with albuterol sulfate",
#' "combivent","duoneb","albuterol","proventil","proair","ventolin","fluticasone","salmeterol","advair",
#' "budesonide/formoterol","budesonide / formoterol","budesonide - formoterol","budesonide-formoterol",
#' "mometasone/formoterol","mometasone / formoterol","dulera","beclomethaosne","qvar","budesonide","pulmicort",
#' "fluticasone","flovent","mometasone","asmanex"
#' @export
copd <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("491","491.0","491.1","491.2","491.20","491.21","491.22","491.8","491.9","492","492.0","492.8","496","496.0","493.21","493.22")
    icd10_codes <- c("J44.%","J43.%","J42","J41.%")
    meds <- c("roflumilast","daliresp","tiotropium","spiriva","ipratropium","atrovent","theophylline","slo-bid","slo-phyllin","theo-dur","theo 24","theo24","theo-24","uniphyl","salmeterol","serevent","formoterol","foradil","albuterol + ipratropium","albuterol / ipratropium","ipratropium bromide / albuterol sulfate","ipratropium bromide with albuterol sulfate","combivent","duoneb","albuterol","proventil","proair","ventolin","fluticasone","salmeterol","advair","budesonide/formoterol","budesonide / formoterol","budesonide - formoterol","budesonide-formoterol","mometasone/formoterol","mometasone / formoterol","dulera","beclomethaosne","qvar","budesonide","pulmicort","fluticasone","flovent","mometasone","asmanex")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_med <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result_icd <- rbind(result_icd9,result_icd10)
    result_icd_agg <- result_icd[,.(icd_status = length(condition_start_date) > 1,
                                            icd_entry_date = min(condition_start_date)),
                                        .(person_id)]
    result_med_agg <- result_med[,.(med_status = length(drug_exposure_start_date) > 0,
                                            med_entry_date = min(drug_exposure_start_date)),
                                        .(person_id)]
    result_all <- merge(result_icd_agg,result_med_agg,by="person_id",all.x=TRUE)
    result_all <- result_all[,.(copd_entry_date = min(c(icd_entry_date,med_entry_date)),
                                copd_status = med_status & icd_status),
                            .(person_id)]
    result_all[,copd_status := ifelse(is.na(copd_status),FALSE,copd_status)]
    .write_to_bucket(result_all,output_folder,"copd")
}
