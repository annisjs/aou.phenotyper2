#' SGLT2 Inhibitors
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sglt2_inhibitors.csv
#' @details Meds: "empagliflozin","jardiance","canagliflozin","invokana","dapagliflozin","farxiga","ertugliflozin","steglatro"
#' @export
sglt2_inhibitors <- function(output_path,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds <- c("empagliflozin","jardiance","canagliflozin","invokana","dapagliflozin","farxiga","ertugliflozin","steglatro")
    result <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result <- result[,.(sglt2_inhibitors_entry_date = min(drug_exposure_start_date)),.(person_id)]
    .write_to_bucket(result,output_folder,"sglt2_inhibitors")
}
