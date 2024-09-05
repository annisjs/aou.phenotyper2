#' All cabg codes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_cabg_codes.csv
#' CPT: "33510","33511","33512","33513","33514","33516","35600","33533","33534","33535","33536","1006199","1006200","1006208","1006216","1006217","4110F","33523","33522","33521","33519","33518","33517"
#' @export
all_cabg_codes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    cpt_codes <- c("33510","33511","33512","33513","33514","33516","35600","33533","33534",
    "33535","33536","1006199","1006200","1006208","1006216","1006217","4110F","33523","33522",
    "33521","33519","33518","33517"
    )
    result_cpt <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
    colnames(result_cpt) <- c("person_id","all_cabg_codes_entry_date","all_cabg_codes_value")
    .write_to_bucket(result_cpt,output_folder,"all_cabg_codes")
}