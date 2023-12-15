#' Closest LDL
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Searches for
#'
#' "Cholesterol in LDL \[Mass/volume] in Serum or Plasma by calculation"
#'
#' "Cholesterol in LDL \[Mass/volume] in Serum or Plasma"
#'
#' "Cholesterol in LDL \[Mass/volume] in Serum or Plasma by Direct assay"
#'
#' "Cholesterol in LDL \[Mass/volume] in Serum or Plasma by Electrophoresis"
#' @return output_folder/closest_ldl.csv
#' @export
closest_ldl <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    lab_terms <- c("Cholesterol in LDL [Mass/volume] in Serum or Plasma by calculation",
                    "Cholesterol in LDL [Mass/volume] in Serum or Plasma",
                    "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Direct assay",
                    "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Electrophoresis")
    result_all <- aou.reader::lab_query(lab_terms,anchor_date_table,before,after)
    result_all[,diff := abs(as.numeric(as.Date(measurement_date) - as.Date(anchor_date)))]
    result_all <- result_all[order(diff)]
    result_all <- result_all[,.(closest_ldl_entry_date = measurement_date[1],
                                closest_ldl_value = value_as_number[1]),.(person_id,anchor_date)]
    .write_to_bucket(result_all,output_folder,"closest_ldl")
}
