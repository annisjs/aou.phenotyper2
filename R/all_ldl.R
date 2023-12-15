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
#' @return output_folder/all_ldl.csv
#' @export
all_ldl <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    result_all <- aou.reader::lab_query(c("Cholesterol in LDL [Mass/volume] in Serum or Plasma by calculation",
                                        "Cholesterol in LDL [Mass/volume] in Serum or Plasma",
                                        "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Direct assay",
                                        "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Electrophoresis"),
                                        anchor_date_table,before,after)
    colnames(result_all) <- c("person_id","all_ldl_entry_date","all_ldl_value")
    .write_to_bucket(result_all,output_folder,"all_ldl")
}
