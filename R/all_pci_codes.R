#' All pci codes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_pci_codes.csv
#' CPT: "92944","92943","92941","92938","92937","92934","92933","92929","92928","92924","92925","92921","92920","37247",
#' "37246","1029697","1021168","1021167","1021166","1021165","1021164","1021163","0715T","0659T"
#' @export
all_pci_codes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    cpt_codes <- c( "92944","92943","92941","92938","92937","92934","92933","92929","92928","92924","92925","92921","92920","37247",
                    "37246","1029697","1021168","1021167","1021166","1021165","1021164","1021163","0715T","0659T"
    )
    result_cpt <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)
    colnames(result_cpt) <- c("person_id","all_pci_codes_value","all_pci_codes_entry_date")
    .write_to_bucket(result_cpt,output_folder,"all_pci_codes")
}