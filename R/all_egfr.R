#' All eGFR
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
#' @return output_folder/all_egfr.csv
#' @export
all_egfr <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    lab_terms <- c("Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted among blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (MDRD)",
                   "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood",
                   "Glomerular filtration rate/1.73 sq M.predicted [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)",
                   "Glomerular filtration rate/1.73 sq M.predicted among non-blacks [Volume Rate/Area] in Serum, Plasma or Blood by Creatinine-based formula (CKD-EPI)")
    result_all <- aou.reader::lab_query(lab_terms,anchor_date_table,before,after)
    colnames(result_all) <- c("person_id","all_egfr_entry_date","all_egfr_value")
    .write_to_bucket(result_all,output_folder,"all_egfr")
}
