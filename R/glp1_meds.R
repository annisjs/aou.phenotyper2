#' GLP1 Meds
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return CSV saved to output_folder/glp1_meds.csv
#' @details Meds: "lixisenatide","adlyxin","albiglutide","tanzeum","exenatide","bydureon bcise","bydureon","byetta","liraglutide","saxenda","victoza",
#'    "dulaglutide","trulicity","semaglutide","ozempic","rybelsus","tirzepatide","mounjaro","zepbound"
#' @export
glp1_meds <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds <- c("lixisenatide","adlyxin","albiglutide","tanzeum","exenatide","bydureon bcise","bydureon","byetta","liraglutide","saxenda","victoza",
    "dulaglutide","trulicity","semaglutide","ozempic","rybelsus","tirzepatide","mounjaro","zepbound")
    result <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result <- result[order(drug_exposure_start_date)]
    result <- result[,.(glp1_meds_entry_date = drug_exposure_start_date[1],
                        glp1_meds_status = TRUE),
                      .(person_id)]
    .write_to_bucket(result,output_folder,"glp1_meds")
}

