#' BMI and eGFR Two-Year Follow-up
#'
#' @param output_folder the folder to write the output
#' @return output_folder/bayer_landmark.csv
#' @details
#' For each participant, defines:
#' t0 = date of first BMI,
#' t1 = t0 + 1 year,
#' t2 = t1 + 1 year.
#'
#' Sets `bayer_landmark` to TRUE when participants have:
#' - at least 1 BMI in (t0, t1]
#' - at least 1 BMI in (t1, t2]
#' - at least 1 eGFR in [t0, t2]
#' @import data.table aou.reader
#' @export
bayer_landmark <- function(output_folder)
{
    bmi_dt <- data.table::as.data.table(aou.reader::bmi_query())

    if (nrow(bmi_dt) == 0)
    {
        empty <- data.table::data.table(
            person_id = character(),
            bayer_landmark_t0_date = as.Date(character()),
            bayer_landmark_t1_date = as.Date(character()),
            bayer_landmark_t2_date = as.Date(character()),
            bayer_landmark_bmi_n_t0_t1 = integer(),
            bayer_landmark_bmi_n_t1_t2 = integer(),
            bayer_landmark_egfr_n_t0_t2 = integer(),
            bayer_landmark = logical()
        )
        .write_to_bucket(empty, output_folder, "bayer_landmark")
        return(invisible(NULL))
    }

    bmi_dt <- bmi_dt[!is.na(person_id) & !is.na(measurement_date)]
    bmi_dt[, measurement_date := as.Date(measurement_date)]
    data.table::setorder(bmi_dt, person_id, measurement_date)

    anchors <- bmi_dt[, .(t0 = measurement_date[1]), by = .(person_id)]

    # Use POSIXlt year increments to keep month/day alignment and leap-year behavior.
    anchors[, t1 := {
        x <- as.POSIXlt(t0)
        x$year <- x$year + 1
        as.Date(x)
    }]
    anchors[, t2 := {
        x <- as.POSIXlt(t1)
        x$year <- x$year + 1
        as.Date(x)
    }]

    bmi_window_dt <- merge(
        bmi_dt[, .(person_id, bmi_date = measurement_date)],
        anchors,
        by = "person_id",
        allow.cartesian = TRUE
    )

    bmi_counts <- bmi_window_dt[, .(
        bmi_n_t0_t1 = sum(bmi_date > t0 & bmi_date <= t1, na.rm = TRUE),
        bmi_n_t1_t2 = sum(bmi_date > t1 & bmi_date <= t2, na.rm = TRUE)
    ), by = .(person_id, t0, t1, t2)]

    lab_terms <- c(
        "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
        "Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
        "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
        "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood",
        "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)",
        "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)"
    )

    egfr_dt <- data.table::as.data.table(aou.reader::lab_query(lab_terms))
    egfr_dt <- egfr_dt[!is.na(person_id) & !is.na(measurement_date)]
    egfr_dt[, measurement_date := as.Date(measurement_date)]

    egfr_window_dt <- merge(
        anchors[, .(person_id, t0, t2)],
        egfr_dt[, .(person_id, egfr_date = measurement_date)],
        by = "person_id",
        all.x = TRUE,
        allow.cartesian = TRUE
    )

    egfr_counts <- egfr_window_dt[, .(
        egfr_n_t0_t2 = sum(egfr_date >= t0 & egfr_date <= t2, na.rm = TRUE)
    ), by = .(person_id, t0, t2)]

    out <- merge(
        bmi_counts,
        egfr_counts,
        by = c("person_id", "t0", "t2"),
        all.x = TRUE
    )

    out[, bayer_landmark := data.table::fifelse(
        bmi_n_t0_t1 >= 1 & bmi_n_t1_t2 >= 1 & egfr_n_t0_t2 >= 1,
        TRUE,
        FALSE
    )]

    data.table::setnames(out,
        old = c("t0", "t1", "t2", "bmi_n_t0_t1", "bmi_n_t1_t2", "egfr_n_t0_t2"),
        new = c(
            "bayer_landmark_t0_date",
            "bayer_landmark_t1_date",
            "bayer_landmark_t2_date",
            "bayer_landmark_bmi_n_t0_t1",
            "bayer_landmark_bmi_n_t1_t2",
            "bayer_landmark_egfr_n_t0_t2"
        )
    )

    .write_to_bucket(out, output_folder, "bayer_landmark")
}