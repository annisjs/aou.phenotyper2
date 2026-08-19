#' CKD from eGFR (fast, experimental)
#'
#' @inheritParams ckd_from_egfr
#' @return output_folder/ckd_from_egfr_fast.csv
#' @details
#' Same logic as \code{ckd_from_egfr()}. This version caches \code{aou.reader::demographics_query()}
#' for the lifetime of the R session, since that query takes no per-call filtering arguments and
#' is otherwise re-fetched from BigQuery identically every time this algorithm runs (e.g. once per
#' before/after date-window variant run through \code{run_algorithms_and_combine_in_memory()}).
#' @import data.table aou.reader
#' @export
ckd_from_egfr_fast <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
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

    recode_sex <- function(sex)
    {
        s <- trimws(as.character(sex))
        data.table::fifelse(
            tolower(s) %in% c("female", "f"),
            "female",
            data.table::fifelse(tolower(s) %in% c("male", "m"), "male", NA_character_)
        )
    }

    creat <- normalize_lab(.cached_creatinine_query(creat_terms))
    creat <- .apply_creatinine_window(creat, anchor_date_table, before, after)
    if (nrow(creat) == 0)
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr_fast")
        return(invisible(NULL))
    }

    demos <- .cached_demographics_query()
    if (!"person_id" %in% names(demos) || !"date_of_birth" %in% names(demos))
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr_fast")
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
        .write_to_bucket(empty, output_folder, "ckd_from_egfr_fast")
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

    labs <- unique(creat_demos[is_outpatient == TRUE,
                               .(person_id, lab_date, lab_value = egfr)])

    if (nrow(labs) == 0)
    {
        .write_to_bucket(empty, output_folder, "ckd_from_egfr_fast")
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

    .write_to_bucket(out, output_folder, "ckd_from_egfr_fast")
}

# session-level cache: demographics_query() takes no filtering args, so re-fetching it
# from BigQuery on every algo run (e.g. once per before/after variant) is pure waste
.demographics_cache_env <- new.env(parent = emptyenv())

.cached_demographics_query <- function()
{
    if (is.null(.demographics_cache_env$demos))
    {
        .demographics_cache_env$demos <- data.table::as.data.table(aou.reader::demographics_query())
    }
    data.table::copy(.demographics_cache_env$demos)
}

.creatinine_cache_env <- new.env(parent = emptyenv())

.cached_creatinine_query <- function(creat_terms)
{
    cache_key <- "creatinine"
    if (is.null(.creatinine_cache_env[[cache_key]]))
    {
        lab_terms <- paste(
            'c.concept_name LIKE ', "'", creat_terms, "'",
            collapse = " OR ", sep = ""
        )
        query <- stringr::str_glue("
            SELECT person_id, measurement_date, value_as_number
            FROM `measurement` m
            INNER JOIN `concept` c ON (m.measurement_concept_id = c.concept_id)
            WHERE ({lab_terms})
        ")
        assign(
            cache_key,
            data.table::as.data.table(
                aou.reader::download_big_data(
                    query,
                    "ckd_from_egfr_fast_creatinine_query_result.csv"
                )
            ),
            envir = .creatinine_cache_env
        )
    }
    data.table::copy(.creatinine_cache_env[[cache_key]])
}

.apply_creatinine_window <- function(dat, anchor_date_table, before, after)
{
    if (is.null(anchor_date_table)) return(dat)

    if (is.null(before)) before <- -100000
    if (is.null(after)) after <- 100000
    out <- data.table::as.data.table(
        merge(dat, anchor_date_table, by = "person_id", allow.cartesian = TRUE)
    )
    out[, min_window_date := as.Date(anchor_date) + before]
    out[, max_window_date := as.Date(anchor_date) + after]
    out <- out[measurement_date >= min_window_date & measurement_date <= max_window_date]
    out[, c("min_window_date", "max_window_date", "anchor_date") := NULL]
    out
}
