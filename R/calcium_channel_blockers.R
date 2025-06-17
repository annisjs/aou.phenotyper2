#' Calcium channel blockers
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return CSV saved to output_folder/calcium_channel_blockers.csv
#' @details Meds: "amlodipine","norvasc","diltiazem","cardizem","tiazac","felodipine","isradipine",
#' "nicardipine","cardenesr","cardene sr","cardene-sr","nifedipine","procardia","nisoldipine","sular",
#' "verapamil","calan","verelan","covera-hs","covera hs","coverahs","lotrel","caduet","bepridil","vascor",
#' "clevidipine","cleviprex","isoptin","plendil","lacidipine","caldine","lacimen","lacipil","midotens","motens",
#' "lercanidipine","lercadip","zanadip","dynacirc","adalat","nifediac","nifedical","nimodipine","nimotop",
#' "prestalia","teczem","lexxel","carmen ace","coripren","tarka","azor","tribenzor","twynsta","exforge","exforge hct"
#' @export
calcium_channel_blockers <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    meds <- c("amlodipine","norvasc","diltiazem","cardizem","tiazac","felodipine","isradipine","nicardipine","cardenesr","cardene sr","cardene-sr","nifedipine","procardia","nisoldipine","sular","verapamil","calan","verelan","covera-hs","covera hs","coverahs","lotrel","caduet","bepridil","vascor","clevidipine","cleviprex","isoptin","plendil","lacidipine","caldine","lacimen","lacipil","midotens","motens","lercanidipine","lercadip","zanadip","dynacirc","adalat","nifediac","nifedical","nimodipine","nimotop","prestalia","teczem","lexxel","carmen ace","coripren","tarka","azor","tribenzor","twynsta","exforge","exforge hct")
    result <- aou.reader::med_query(meds,anchor_date_table,before,after)
    result <- result[,.(calcium_channel_blockers_entry_date = min(drug_exposure_start_date)),.(person_id)]
    .write_to_bucket(result,output_folder,"calcium_channel_blockers")
}

