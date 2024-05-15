#' All newborn deliveries
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_newborn_delivery.csv
#' @details At least 1 CPT code
#'
#' CPT codes for vaginal delivery: "59400", "59409", "59410", "59610", "59612", "59614"
#' CPT codes for cesarean delivery: "59510", "59514", "59515", "59618", "59620", "59622" 
#'
#' @export
all_newborn_deliveries <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    cpt_codes_vaginal <- c("59400", "59409", "59410", "59610", "59612", "59614")
    cpt_codes_cesarean <- c("59510", "59514", "59515", "59618", "59620", "59622")
    result_vaginal <- aou.reader::cpt_query(cpt_codes_vaginal,anchor_date_table,before,after)
    result_vaginal[, newborn_delivery_type := "vaginal_delivery"]
    result_cesarean <- aou.reader::cpt_query(cpt_codes_cesarean,anchor_date_table,before,after)
    result_cesarean[, newborn_delivery_type := "cesarean_delivery"]
    result_all <- rbind(result_vaginal,result_cesarean)
    result_all <- result_all[,c("person_id","entry_date","newborn_delivery_type")]
    colnames(result_all) <- c("person_id","all_newborn_deliveries_entry_date","all_newborn_deliveries_type")
    .write_to_bucket(result_all,output_folder,"all_newborn_deliveries")
}