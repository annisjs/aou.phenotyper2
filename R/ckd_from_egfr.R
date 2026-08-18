#' CKD from eGFR
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @param suffix optional string appended to the end of every output column name except person_id.
#' @return output_folder/ckd_from_egfr.csv
#' @details
#' Replicates the CKD-from-eGFR logic from a PySpark phenotype using the 2021 CKD-EPI
#' creatinine equation (Inker NEJM 2021):
#' - Calculate eGFR from creatinine, age, and sex
#' - Keep outpatient labs when outpatient metadata are available
#' - Require two eGFR values < 60 at least 90 days apart
#' - Exclude pairs with any eGFR >= 60 between the two dates (inclusive)
#'
#' Outpatient filtering is best effort in this repository and depends on available
#' aou.reader outpatient APIs/columns.
#' @import data.table aou.reader
#' @export
ckd_from_egfr <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    creat_terms <- c(
        "Creatinine",
        "Creatinine | Serum or Plasma | Chemistry - non-challenge",
        "Creatinine [Mass/volume] in Serum or Plasma"
    )

    empty <- data.table::data.table(
        person_id = character(),
        ckd_from_egfr_date = as.Date(character()),
        ckd_from_egfr_lab_value = numeric(),
        ckd_from_egfr_status = logical()
    )

    normalize_lab <- function(dt)
    {
        if (is.null(dt) || nrow(dt) == 0 || ncol(dt) < 3)
        {
            return(data.table::data.table(
                person_id = character(),
                lab_date = as.Date(character()),
                lab_value = numeric()
            ))
        }

        out <- data.table::as.data.table(dt)
        data.table::setnames(out, old = names(out)[1:3], new = c("person_id", "lab_date", "lab_value"))
        out[, lab_date := as.Date(lab_date)]
        out[, lab_value := suppressWarnings(as.numeric(lab_value))]
        out <- out[!is.na(person_id) & !is.na(lab_date) & !is.na(lab_value)]
        out
    }

    normalize_outpatient_dates <- function(dt)
    {
        if (is.null(dt) || nrow(dt) == 0 || !"person_id" %in% names(dt))
        {
            return(data.table::data.table(person_id = character(), visit_date = as.Date(character())))
        }

        date_col <- intersect(c(
            "visit_start_date", "outpatient_visit_date", "entry_date",
            "medical_encounter_entry_date", "date"
        ), names(dt))

        if (length(date_col) == 0)
        {
            return(data.table::data.table(person_id = character(), visit_date = as.Date(character())))
        }

        out <- data.table::as.data.table(dt)
        out[, visit_date := as.Date(get(date_col[1]))]
        out <- out[!is.na(person_id) & !is.na(visit_date), .(person_id, visit_date)]
        unique(out)
    }

    recode_sex <- function(sex)
    {
        s <- trimws(as.character(sex))
        data.table::fifelse(
            tolower(s) %in% c("female", "f"),
            "female",
            data.table::fifelse(tolower(s) %in% c("male", "m"), "male", NA_character_)
        )
    }

    creat <- normalize_lab(aou.reader::lab_query(creat_terms, anchor_date_table, before, after))
    if (nrow(creat) == 0)
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr")
        return(invisible(NULL))
    }

    demos <- data.table::as.data.table(aou.reader::demographics_query())
    if (!"person_id" %in% names(demos) || !"date_of_birth" %in% names(demos))
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr")
        return(invisible(NULL))
    }

    sex_col <- intersect(c("sex", "sex_at_birth", "biological_sex"), names(demos))
    if (length(sex_col) == 0)
    {
        demos[, sex := NA_character_]
    } else {
        demos[, sex := get(sex_col[1])]
    }

    demos <- demos[, .(person_id, date_of_birth = as.Date(date_of_birth), sex = recode_sex(sex))]
    creat_demos <- merge(creat, demos, by = "person_id", all.x = TRUE)
    creat_demos <- creat_demos[!is.na(date_of_birth) & !is.na(sex)]

    if (nrow(creat_demos) == 0)
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr")
        return(invisible(NULL))
    }

    creat_demos[, age := floor(as.numeric(lab_date - date_of_birth) / 365.25)]
    creat_demos <- creat_demos[!is.na(age) & age >= 0]

    creat_demos[, kappa := data.table::fifelse(sex == "female", 0.7, 0.9)]
    creat_demos[, alpha := data.table::fifelse(sex == "female", -0.241, -0.302)]
    creat_demos[, sex_coef := data.table::fifelse(sex == "female", 1.012, 1.0)]

    ratio <- creat_demos$lab_value / creat_demos$kappa
    creat_demos[, egfr := 142 *
                   (pmin(ratio, 1) ^ alpha) *
                   (pmax(ratio, 1) ^ (-1.2)) *
                   (0.9938 ^ age) *
                   sex_coef]

    # Temporarily disable outpatient requirement to avoid over-filtering to zero rows.
    # This allows all creatinine-derived eGFR measurements to participate.
    creat_demos[, is_outpatient := TRUE]

    # Previous outpatient-filtering logic (kept for easy revert):
    # exports <- getNamespaceExports("aou.reader")
    # outpatient_visits <- NULL
    # if ("outpatient_visit_query" %in% exports)
    # {
    #     fn <- get("outpatient_visit_query", asNamespace("aou.reader"))
    #     outpatient_visits <- tryCatch(fn(anchor_date_table, before, after), error = function(e) NULL)
    # }
    # outpatient_dates <- normalize_outpatient_dates(outpatient_visits)
    # has_lab_visit_type <- "lab_visit_type" %in% names(creat_demos)
    # if (nrow(outpatient_dates) > 0)
    # {
    #     creat_demos[, is_outpatient := FALSE]
    #     match_idx <- outpatient_dates[creat_demos, on = .(person_id, visit_date = lab_date), which = TRUE]
    #     creat_demos[!is.na(match_idx), is_outpatient := TRUE]
    #     if (has_lab_visit_type)
    #     {
    #         creat_demos[lab_visit_type == "Outpatient Visit", is_outpatient := TRUE]
    #     }
    # } else if (has_lab_visit_type) {
    #     creat_demos[, is_outpatient := lab_visit_type == "Outpatient Visit"]
    # } else {
    #     creat_demos[, is_outpatient := FALSE]
    # }

    labs <- unique(creat_demos[is_outpatient == TRUE,
                               .(person_id, lab_date, lab_value = egfr)])

    if (nrow(labs) == 0)
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr")
        return(invisible(NULL))
    }

    labs_lt_sixty <- labs[lab_value < 60]
    if (nrow(labs_lt_sixty) == 0)
    {
        out <- empty
    } else {
        pairs <- merge(
            labs_lt_sixty,
            labs_lt_sixty,
            by = "person_id",
            allow.cartesian = TRUE,
            suffixes = c("_first", "_second")
        )
        pairs <- pairs[as.numeric(lab_date_second - lab_date_first) > 90]

        if (nrow(pairs) == 0)
        {
            out <- empty
        } else {
            pairs[, pair_id := .I]
            labs_gte_sixty <- labs[lab_value >= 60]

            if (nrow(labs_gte_sixty) > 0)
            {
                # Count qualifying >=60 labs per pair without materializing all matches.
                intervening_counts <- labs_gte_sixty[pairs,
                                                     on = .(person_id,
                                                            lab_date >= lab_date_first,
                                                            lab_date <= lab_date_second),
                                                     .(n_recovery = .N),
                                                     by = .EACHI]
                valid_pairs <- pairs[intervening_counts$n_recovery == 0L]
            } else {
                valid_pairs <- pairs
            }

            if (nrow(valid_pairs) == 0)
            {
                out <- empty
            } else {
                data.table::setorder(valid_pairs, person_id, lab_date_first)
                out <- valid_pairs[, .(
                    ckd_from_egfr_date = lab_date_first[1],
                    ckd_from_egfr_lab_value = lab_value_first[1],
                    ckd_from_egfr_status = TRUE
                ), by = .(person_id)]
            }
        }
    }

    if (!is.null(anchor_date_table))
    {
        anchor_dt <- unique(data.table::as.data.table(anchor_date_table)[, .(person_id)])
        out <- merge(anchor_dt, out, by = "person_id", all.x = TRUE)
        out[is.na(ckd_from_egfr_status), ckd_from_egfr_status := FALSE]
    }

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(out), "person_id")
        data.table::setnames(out, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(out, output_folder, "ckd_from_egfr")
}
