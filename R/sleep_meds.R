#' Sleep meds
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return CSV saved to output_folder/sleep_meds.csv
#' @details Meds: benzodiazepines: temazepam,restoril,lorazepam,ativan,diazepam,valium
#'                non-benzodiazepine_hypnotics: zolpidem,ambien,ambien cr,eszopiclone,lunesta,zaleplon,sonata
#'                melatonin_receptor_agonists: suvorexant,belsomra,lemborexant,dayvigo
#'                antidepressants_off_label: trazodone,desyrel,doxepin,silenor
#'                antihistamines: diphenhydramine,benadryl,nytol,doxylamine,unisom
#'                melatonin: melatonin
#' @export
sleep_meds <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
	med_list <- list("benzodiazepines" = c("temazepam", "restoril", "lorazepam", "ativan", "diazepam", "valium"),
                    "non_benzodiazepine_hypnotics" = c("zolpidem"," ambien","ambien cr", "eszopiclone", "lunesta", "zaleplon", "sonata"),
                    "melatonin_receptor_agonists" = c("suvorexant", "belsomra", "lemborexant", "dayvigo"),
                    "antidepressants_off_label" = c("trazodone", "desyrel", "doxepin", "silenor"),
                    "antihistamines" = c("diphenhydramine", "benadryl", "nytol", "doxylamine", "unisom"),
                    "melatonin" = c("melatonin"),
                    "eszopiclone" = c("lunesta"),
                    "zolpidem" = c("ambien"))
    med_classes <- names(med_list)
	dt_list <- lapply(med_list, aou.reader::med_with_record_source_query, anchor_date_table, before, after)
    dt_list <- Map(cbind, dt_list, med_class = med_classes)
    dt_list <- lapply(dt_list, function(x) x[, drug_exposure_start_date := as.Date(drug_exposure_start_date)])
    dt <- rbindlist(dt_list)
    dt <- dt[order(drug_exposure_start_date)]
    dt <- dt[, row_num := 1:.N, .(person_id, med_class)]
    dt <- dt[row_num == 1]
    dt[, row_num := NULL]
    dt[, status := TRUE]
    dt_cast <- dcast(dt, person_id + record_source  ~ med_class, value.var = c("drug_exposure_start_date","status"))
    med_date_cols <- paste0("drug_exposure_start_date_", med_classes)
    med_status_cols <- paste0("status_", med_classes)
    dt_cast[, sleep_meds_any_entry_date := apply(.SD, 1, min, na.rm = T), .SDcols = med_date_cols]
    dt_cast[, sleep_meds_any_status := TRUE]
    for (c in med_status_cols) dt_cast[, (c) := ifelse(is.na(get(c)), FALSE, get(c))]
    setnames(dt_cast, med_date_cols, paste0("sleep_meds_", med_classes, "_entry_date"))
    setnames(dt_cast, med_status_cols, paste0("sleep_meds_", med_classes, "_status"))
    setnames(dt_cast, "record_source", "sleep_meds_record_source")
	.write_to_bucket(dt_cast, output_folder, "sleep_meds")
}