#' First Medical Encounter
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @param suffix optional string appended to the end of every output column name except person_id.
#'   Defaults to NULL (no renaming).
#' @return output_folder/first_medical_encounter.csv
#' @import stringr bigrquery data.table
#' @export
first_medical_encounter <- function(output_folder, anchor_date_table = NULL, before = NULL, after = NULL, suffix = NULL)
{
    result <- aou.reader::medical_encounter_query("first", anchor_date_table, before, after)
    colnames(result) <- c("person_id", "first_medical_encounter_entry_date")

    result <- data.table::as.data.table(result)

    if (!is.null(suffix) && nzchar(suffix)) {
        cols_to_rename <- setdiff(names(result), "person_id")
        data.table::setnames(result, cols_to_rename, paste0(cols_to_rename, suffix))
    }

    .write_to_bucket(result, output_folder, "first_medical_encounter")
}
