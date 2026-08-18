#' Revascularization
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @return output_folder/revascularization.csv
#' @details At least 1 ICD9, ICD10, or CPT code.
#'
#' ICD9: 410, 412
#'
#' ICD10: I25.2, I21, I22, I23, I25.5, I24.1, I25.6
#'
#' CPT: 33510, 33511, 33512, 33513, 33514, 33515, 33516, 33517, 33518, 33519, 33520, 33521, 33522, 33523, 33534, 33535, 33536, 92920, 92921, 92922, 92923, 92924, 92925, 92926, 92927, 92928, 92929, 92930, 92931, 92932, 92933, 92934, 92935, 92936, 92937, 92938, 92939, 92940, 92941, 92942, 92943, 92944, 92945, 92946, 92947, 92948, 92949, 92950, 92951, 92952, 92953, 92954, 92955, 92956, 92957, 92958, 92959, 92960, 92961, 92962, 92963, 92964, 92965, 92966, 92967, 92968, 92969, 92970, 92971, 92972, 92973, 92974, 92975, 92976, 92977, 92980, 92981, 92982, 92984, 92995, 92996
#' @import data.table aou.reader
#' @export
revascularization <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    icd9_codes <- c("410", "410.%", "412", "412.%")
    icd10_codes <- c(
        "I25.2", "I25.2%",
        "I21", "I21.%",
        "I22", "I22.%",
        "I23", "I23.%",
        "I25.5", "I25.5%",
        "I24.1", "I24.1%",
        "I25.6", "I25.6%"
    )
    cpt_codes <- c(
        "33510", "33511", "33512", "33513", "33514", "33515", "33516", "33517", "33518", "33519",
        "33520", "33521", "33522", "33523", "33534", "33535", "33536",
        "92920", "92921", "92922", "92923", "92924", "92925", "92926", "92927", "92928", "92929",
        "92930", "92931", "92932", "92933", "92934", "92935", "92936", "92937", "92938", "92939",
        "92940", "92941", "92942", "92943", "92944", "92945", "92946", "92947", "92948", "92949",
        "92950", "92951", "92952", "92953", "92954", "92955", "92956", "92957", "92958", "92959",
        "92960", "92961", "92962", "92963", "92964", "92965", "92966", "92967", "92968", "92969",
        "92970", "92971", "92972", "92973", "92974", "92975", "92976", "92977", "92980", "92981",
        "92982", "92984", "92995", "92996"
    )

    result_icd9 <- data.table::as.data.table(aou.reader::icd9_query(icd9_codes, anchor_date_table, before, after))
    result_icd10 <- data.table::as.data.table(aou.reader::icd10_query(icd10_codes, anchor_date_table, before, after))
    result_cpt <- data.table::as.data.table(aou.reader::cpt_query(cpt_codes, anchor_date_table, before, after))

    icd_all <- data.table::rbindlist(list(result_icd9, result_icd10), fill = TRUE)
    icd_agg <- icd_all[, .(
        icd_status = length(condition_start_date) > 0,
        icd_date = min(condition_start_date)
    ), by = .(person_id)]

    cpt_agg <- result_cpt[, .(
        cpt_status = length(entry_date) > 0,
        cpt_date = min(entry_date)
    ), by = .(person_id)]

    result_all <- merge(icd_agg, cpt_agg, by = "person_id", all = TRUE)
    result_all[is.na(icd_status), icd_status := FALSE]
    result_all[is.na(cpt_status), cpt_status := FALSE]

    result_all[, revascularization_status := icd_status | cpt_status]
    result_all[, revascularization_date := as.Date(NA)]
    result_all[revascularization_status == TRUE,
               revascularization_date := as.Date(
                   pmin(as.Date(icd_date), as.Date(cpt_date), na.rm = TRUE),
                   origin = "1970-01-01"
               )]

    out <- result_all[, .(person_id, revascularization_status, revascularization_date)]

    if (!is.null(anchor_date_table))
    {
        anchor_dt <- unique(data.table::as.data.table(anchor_date_table)[, .(person_id)])
        out <- merge(anchor_dt, out, by = "person_id", all.x = TRUE)
        out[is.na(revascularization_status), revascularization_status := FALSE]
    }

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "revascularization")
}
