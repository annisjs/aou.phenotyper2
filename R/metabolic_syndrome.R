#' Metabolic Syndrome
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/metabolic_syndrome.csv
#' @details
#' Metabolic syndrome status is TRUE when at least 2 of the following are TRUE:
#' HTN, DM, Dyslipidemia.
#'
#' HTN:
#' - At least 1 ICD9/ICD10 code OR hypertension problem-list keyword.
#'
#' DM:
#' - At least 1 ICD9/ICD10 code AND diabetes problem-list keyword.
#'
#' Dyslipidemia:
#' - Chol > 220 OR Trig > 200 OR HDL < 40 OR (HDL < 45 AND female)
#'   OR at least 1 lipid-lowering medication.
#'
#' Problem-list matching is best effort and depends on available aou.reader APIs.
#' @import data.table aou.reader
#' @export
metabolic_syndrome <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL)
{
    get_min_date <- function(dt)
    {
        if (is.null(dt) || nrow(dt) == 0)
        {
            return(data.table::data.table(person_id = character(), event_date = as.Date(character())))
        }

        date_col <- intersect(c(
            "condition_start_date", "entry_date", "drug_exposure_start_date",
            "measurement_date", "observation_date", "event_date", "date"
        ), names(dt))

        if (length(date_col) == 0 || !"person_id" %in% names(dt))
        {
            return(data.table::data.table(person_id = character(), event_date = as.Date(character())))
        }

        out <- data.table::as.data.table(dt)
        out[, event_date := as.Date(get(date_col[1]))]
        out <- out[!is.na(person_id) & !is.na(event_date)]

        if (nrow(out) == 0)
        {
            return(data.table::data.table(person_id = character(), event_date = as.Date(character())))
        }

        out[, .(event_date = min(event_date)), by = .(person_id)]
    }

    problem_list_query_best_effort <- function(keywords)
    {
        exports <- getNamespaceExports("aou.reader")

        if ("problem_list_query" %in% exports)
        {
            fn <- get("problem_list_query", asNamespace("aou.reader"))
            res <- tryCatch(fn(keywords, anchor_date_table, before, after), error = function(e) NULL)
            return(data.table::as.data.table(res))
        }

        if ("problem_query" %in% exports)
        {
            fn <- get("problem_query", asNamespace("aou.reader"))
            res <- tryCatch(fn(keywords, anchor_date_table, before, after), error = function(e) NULL)
            return(data.table::as.data.table(res))
        }

        data.table::data.table()
    }

    recode_sex_fm <- function(sex)
    {
        ifelse(
            sex == "Not male, not female, prefer not to answer, or skipped" |
                sex == "No matching concept" |
                is.na(sex),
            NA,
            ifelse(sex == "Male", "Male", "Female")
        )
    }

    normalize_lab <- function(dt)
    {
        if (is.null(dt) || nrow(dt) == 0 || ncol(dt) < 3)
        {
            return(data.table::data.table(
                person_id = character(),
                measurement_date = as.Date(character()),
                value_as_number = numeric()
            ))
        }

        out <- data.table::as.data.table(dt)
        data.table::setnames(out, old = names(out)[1:3], new = c("person_id", "measurement_date", "value_as_number"))
        out
    }

    # HTN: ICD OR problem list
    htn_icd9_codes <- c("401", "401.%", "402", "402.%", "403", "403.%", "404", "404.%", "405", "405.%")
    htn_icd10_codes <- c("I10", "I10.%", "I11", "I11.%", "I12", "I12.%", "I13", "I13.%", "I16", "I16.%")
    htn_problem_terms <- c("hypertension", "htn")

    htn_icd9 <- data.table::as.data.table(aou.reader::icd9_query(htn_icd9_codes, anchor_date_table, before, after))
    htn_icd10 <- data.table::as.data.table(aou.reader::icd10_query(htn_icd10_codes, anchor_date_table, before, after))
    htn_problem <- problem_list_query_best_effort(htn_problem_terms)

    htn_dates <- data.table::rbindlist(list(
        get_min_date(htn_icd9),
        get_min_date(htn_icd10),
        get_min_date(htn_problem)
    ), fill = TRUE)
    htn_dates <- htn_dates[, .(htn_date = min(event_date)), by = .(person_id)]
    htn_dates[, htn_status := TRUE]

    # DM: (ICD9 OR ICD10) AND problem list
    dm_icd9_codes <- c("250", "250.%", "V58.67")
    dm_icd10_codes <- c("Z79.4", "Z79.84", "E08", "E08.%", "E09", "E09.%", "E10", "E10.%", "E11", "E11.%", "E13", "E13.%")
    dm_problem_terms <- c("dm", "diabetes")

    dm_icd9 <- data.table::as.data.table(aou.reader::icd9_query(dm_icd9_codes, anchor_date_table, before, after))
    dm_icd10 <- data.table::as.data.table(aou.reader::icd10_query(dm_icd10_codes, anchor_date_table, before, after))
    dm_problem <- problem_list_query_best_effort(dm_problem_terms)

    dm_icd_dates <- data.table::rbindlist(list(get_min_date(dm_icd9), get_min_date(dm_icd10)), fill = TRUE)
    dm_icd_dates <- dm_icd_dates[, .(dm_icd_date = min(event_date)), by = .(person_id)]
    dm_problem_dates <- get_min_date(dm_problem)
    data.table::setnames(dm_problem_dates, "event_date", "dm_problem_date")

    dm_dates <- merge(dm_icd_dates, dm_problem_dates, by = "person_id", all = FALSE)
    dm_dates[, dm_date := pmin(dm_icd_date, dm_problem_date, na.rm = TRUE)]
    dm_dates[, dm_status := TRUE]
    dm_dates <- dm_dates[, .(person_id, dm_status, dm_date)]

    # Dyslipidemia by labs and meds
    chol_dt <- normalize_lab(aou.reader::lab_query("Cholesterol [Mass/volume] in Serum or Plasma", anchor_date_table, before, after))
    trigs_dt <- normalize_lab(aou.reader::lab_query(c("Triglyceride [Mass/volume] in Serum or Plasma", "Triglyceride [Mass/volume] in Blood"), anchor_date_table, before, after))
    hdl_dt <- normalize_lab(aou.reader::lab_query("Cholesterol in HDL [Mass/volume] in Serum or Plasma", anchor_date_table, before, after))

    chol_dt[, measurement_date := as.Date(measurement_date)]
    trigs_dt[, measurement_date := as.Date(measurement_date)]
    hdl_dt[, measurement_date := as.Date(measurement_date)]

    chol_dt[, value_as_number := suppressWarnings(as.numeric(value_as_number))]
    trigs_dt[, value_as_number := suppressWarnings(as.numeric(value_as_number))]
    hdl_dt[, value_as_number := suppressWarnings(as.numeric(value_as_number))]

    chol_hit <- chol_dt[value_as_number > 220 & !is.na(measurement_date), .(dyslipidemia_date = min(measurement_date)), by = .(person_id)]
    trigs_hit <- trigs_dt[value_as_number > 200 & !is.na(measurement_date), .(dyslipidemia_date = min(measurement_date)), by = .(person_id)]

    dem <- data.table::as.data.table(aou.reader::demographics_query())
    sex_col <- intersect(c("sex", "sex_at_birth", "biological_sex"), names(dem))
    if (length(sex_col) > 0)
    {
        dem <- dem[, .(person_id, sex = get(sex_col[1]))]
        dem[, sex := recode_sex_fm(sex)]
    } else {
        dem <- data.table::data.table(person_id = character(), sex = character())
    }

    hdl_merged <- merge(hdl_dt, dem, by = "person_id", all.x = TRUE)
    hdl_merged[, hdl_hit := value_as_number < 40 | (value_as_number < 45 & sex == "Female")]
    hdl_hit <- hdl_merged[hdl_hit == TRUE & !is.na(measurement_date), .(dyslipidemia_date = min(measurement_date)), by = .(person_id)]

    lipid_meds <- c(
        "atorvastatin", "lipitor", "torvast", "lovastatin", "altocor", "pravastatin", "pravachol", "rosuvastatin",
        "crestor", "simvastatin", "zocor", "cholestyramine", "prevalite", "colestopil", "colestid", "colesevelam",
        "welchol", "niacin", "niacor", "niaspan", "gemfibrozil", "lopid", "fenofibrate", "tricor", "fibrocor",
        "bezafibrate", "bezalip", "ezetimibe", "zetia"
    )
    dyslipidemia_meds <- data.table::as.data.table(aou.reader::med_query(lipid_meds, anchor_date_table, before, after))
    med_hit <- get_min_date(dyslipidemia_meds)
    data.table::setnames(med_hit, "event_date", "dyslipidemia_date")

    dyslipidemia_dates <- data.table::rbindlist(list(chol_hit, trigs_hit, hdl_hit, med_hit), fill = TRUE)
    dyslipidemia_dates <- dyslipidemia_dates[, .(dyslipidemia_date = min(dyslipidemia_date)), by = .(person_id)]
    dyslipidemia_dates[, dyslipidemia_status := TRUE]

    # Combine components and apply >=2 rule
    result <- merge(htn_dates[, .(person_id, htn_status, htn_date)],
                    dm_dates,
                    by = "person_id",
                    all = TRUE)
    result <- merge(result,
                    dyslipidemia_dates[, .(person_id, dyslipidemia_status, dyslipidemia_date)],
                    by = "person_id",
                    all = TRUE)

    result[is.na(htn_status), htn_status := FALSE]
    result[is.na(dm_status), dm_status := FALSE]
    result[is.na(dyslipidemia_status), dyslipidemia_status := FALSE]

    result[, component_n := as.integer(htn_status) + as.integer(dm_status) + as.integer(dyslipidemia_status)]
    result[, metabolic_syndrome_status := component_n >= 2]
    result[, metabolic_syndrome_entry_date := pmin(htn_date, dm_date, dyslipidemia_date, na.rm = TRUE)]
    result[metabolic_syndrome_status == FALSE, metabolic_syndrome_entry_date := as.Date(NA)]

    out <- result[, .(person_id, metabolic_syndrome_status, metabolic_syndrome_entry_date)]

    if (!is.null(anchor_date_table))
    {
        anchor_dt <- unique(data.table::as.data.table(anchor_date_table)[, .(person_id)])
        out <- merge(anchor_dt, out, by = "person_id", all.x = TRUE)
        out[is.na(metabolic_syndrome_status), metabolic_syndrome_status := FALSE]
    }

    .write_to_bucket(out, output_folder, "metabolic_syndrome")
}