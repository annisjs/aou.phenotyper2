#' Diuretics
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return CSV saved to output_folder/diuretics.csv
#' @details Meds:"furosemide","lasix","bumetanide","bumex","torsemide","demadex","ethacrynic acid","edecrin","metolazone",
#'            "zaroxolyn","spironolactone","aldactone","hydrochlorothiazide","microzide","amiloride", "midamor",
#'            "triamterene","dyrenium","eplerenone","inspra"
#' @export
diuretics <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds <- c("furosemide","lasix","bumetanide","bumex","torsemide","demadex","ethacrynic acid","edecrin","metolazone",
            "zaroxolyn","spironolactone","aldactone","hydrochlorothiazide","microzide","amiloride", "midamor",
            "triamterene","dyrenium","eplerenone","inspra")
    result <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result <- result[,.(diuretics_entry_date = min(drug_exposure_start_date)),.(person_id)]
    .write_to_bucket(result,output_folder,"diuretics")
}

