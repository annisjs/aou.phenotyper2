#' Beta blockers
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/beta_blockers.csv
#' @details Meds: "betaxolol", "kerlone", "betoptic", "acebutolol", "sectral",
#' "atenolol", "tenormin", "metoprolol", "metoprolol succinate",
#' "toprol-xl", "metoprolol tartrate", "lopressor", "metoprolol succinate er",
#' "metoprolol tartrate", "dutoprolol", "nebivolol", "bystolic", "bisoprolol", "zebeta", "esmolol",
#' "brevibloc", "propranolol", "innopran xl", "inderal", "inderal la", "nadolol",
#' "corgard", "carvedilol", "coreg", "coreg cr", "labetalol", "normodyne",
#' "trandate", "timolol", "blocadren", "timoptic", "istalol", "betimol", "carteolol",
#' "cartrol", "penbutolol", "levatol", "pindolol", "visken"
#' @export
beta_blockers <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds <- c("betaxolol", "kerlone", "betoptic", "acebutolol", "sectral", "atenolol", "tenormin", "metoprolol", "metoprolol succinate", "toprol-xl", "metoprolol tartrate", "lopressor", "metoprolol succinate er", "metoprolol tartrate", "dutoprolol", "nebivolol", "bystolic", "bisoprolol", "zebeta", "esmolol", "brevibloc", "propranolol", "innopran xl", "inderal", "inderal la", "nadolol", "corgard", "carvedilol", "coreg", "coreg cr", "labetalol", "normodyne", "trandate", "timolol", "blocadren", "timoptic", "istalol", "betimol", "carteolol", "cartrol", "penbutolol", "levatol", "pindolol", "visken")
    result <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result <- result[,.(beta_blockers_entry_date = min(drug_exposure_start_date)),.(person_id)]
    .write_to_bucket(result,output_folder,"beta_blockers")
}
