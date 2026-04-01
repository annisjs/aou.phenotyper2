#' All GLP1 Meds
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return CSV saved to output_folder/all_glp1_meds.csv
#' @details Meds: "lixisenatide","adlyxin","albiglutide","tanzeum","exenatide","bydureon bcise","bydureon","byetta","liraglutide","saxenda","victoza",
#'    "dulaglutide","trulicity","semaglutide","ozempic","rybelsus","tirzepatide","mounjaro","zepbound"
#' @export
all_glp1_meds <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds = list(
        "lixisenatide" = c("lixisenatide","adlyxin"),
        "albiglutide" = c("albiglutide","tanzeum","eperzan"),
        "exenatide" = c("exenatide","bydureon","byetta"),
        "liraglutide" = c("liraglutide","saxenda","victoza"),
        "dulaglutide" = c("dulaglutide","trulicity"),
        "semaglutide" = c("semaglutide","ozempic","rybelsus","wegovy"),
        "tirzepatide" = c("tirzepatide","mounjaro","zepbound")
    )
    result <- NULL
    for (m in names(meds))
    {
        tmp <- aou.reader::med_query(meds[[m]], anchor_date_table, before, after)
        tmp[, all_glp1_meds_generic_name := m]
        result <- rbind(tmp, result)
    }
    data.table::setnames(result, "drug_exposure_start_date", "all_glp1_meds_entry_date")
    data.table::setnames(result, "drug_name", "all_glp1_meds_name")
    result <- result[, c("person_id", "all_glp1_meds_entry_date", "all_glp1_meds_generic_name", "all_glp1_meds_name")]
    .write_to_bucket(result,output_folder,"all_glp1_meds")
}
