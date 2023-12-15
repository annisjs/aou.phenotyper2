#' Closest NT-PRO-BNP
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#'
#' "Natriuretic peptide.B prohormone N-Terminal \[Mass/volume] in Serum or Plasma"
#' @return output_folder/closest_pro_bnp.csv
#' @export
closest_pro_bnp <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    if (is.null(anchor_date_table))
    {
        stop("closest_pro_bnp is not a primary variable and requires an anchor date table.")
    }
    lab_terms <- "Natriuretic peptide.B prohormone N-Terminal [Mass/volume] in Serum or Plasma"
    result_all <- aou.reader::lab_query(lab_terms,anchor_date_table,before,after)
    result_all <- as.data.table(merge(result_all,anchor_date_table,by="person_id"))
    result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
    result_all <- result_all[order(diff)]
    result_all <- result_all[,.(closest_pro_bnp_entry_date = measurement_date[1],
                                closest_pro_bnp_value = value_as_number[1]),.(person_id,anchor_date)]
    .write_to_bucket(result_all,output_folder,"closest_pro_bnp")
}
