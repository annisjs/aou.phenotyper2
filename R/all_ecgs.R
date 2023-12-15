#' All ECGs
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for ECG measurements
#' @export
all_ecgs <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result_all <- aou.reader::ecg_query(anchor_date_table,before,after)
    coln <- colnames(result_all)
    coln <- coln[which(coln != "person_id")]
    coln <- paste0("all_ecgs_",coln)
    colnames(result_all) <- c("person_id",coln)
    .write_to_bucket(result_all,output_folder,"all_ecgs")
}

