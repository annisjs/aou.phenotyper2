#' PCI Bypass
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/pci_bypass.csv
#' @details At least 1 ICD procedure code
#'
#' ICD Procedure: 0210%, 0211%, 0212%, 0213%, 0270%, 0271%, 0272%, 0273%
#'
#' @export
pci_bypass <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd_codes <- c("0210%", "0211%", "0212%", "0213%", "0270%", "0271%", "0272%", "0273%")
    result <- aou.reader::icd_procedure_query(icd_codes,anchor_date_table,before,after)
    result <- result[order(icd_procedure_date)]
    result <- result[,.(pci_bypass_status = TRUE,
                                pci_bypass_entry_date = icd_procedure_date[1]),
                            .(person_id)]
    .write_to_bucket(result,output_folder,"pci_bypass")
}
