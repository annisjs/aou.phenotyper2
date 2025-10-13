#' Closest eGFR
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#' "Glomerular filtration rate/1.73 sq M.predicted \\[Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)"
#' "Glomerular filtration rate/1.73 sq M.predicted among blacks \\[Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)"
#' "Glomerular filtration rate/1.73 sq M.predicted among non-blacks \\[Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)"
#' "Glomerular filtration rate/1.73 sq M.predicted \\[Volume Rate/Area] in Serum, Plasma or Blood"
#' "Glomerular filtration rate/1.73 sq M.predicted \\[Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)"
#' "Glomerular filtration rate/1.73 sq M.predicted among non-blacks \\[Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)"
#' @return output_folder/closest_egfr.csv
#' @export
closest_egfr <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    if (is.null(anchor_date_table))
    {
    stop("closest_egfr is not a primary variable and requires an anchor date table.")
    }
    lab_terms <- c("Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood",
                   "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)",
                   "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)")
    result_all <- aou.reader::lab_query(lab_terms,anchor_date_table,before,after)
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
    result_all <- result_all[order(diff)]
    result_all <- result_all[,.(closest_egfr_entry_date = measurement_date[1],
                                closest_egfr_value = value_as_number[1]),
                            .(person_id,anchor_date)]
    .write_to_bucket(result_all,output_folder,"closest_egfr")
}
