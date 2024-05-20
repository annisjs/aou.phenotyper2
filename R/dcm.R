#' DCM
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dcm.csv
#' @details At least 1 ICD code:
#'
#' ICD9: "410","410.%","411","411.%"
#'
#' ICD10: "I21","I21.%","I22","I22.%","I23","I23.%","I24","I24.%","I25","I25.%"
#' 
#' Snomed: "85898001","52029003","471890009","195021004","53043001",
#'              "204981000119101","30496006","74249003","111285003","1149109001","16253001",
#'              "72972005","713523008","880042006","1234750000","12563008","766883006",
#'              "471841009","62377009","16253001","153941000119100","417996009","703275009",
#'              "703272007","703274008","443254009","153931000119109","443253003","441481004",
#'              "120871000119108","120861000119102","120851000119104","1064941000000100" 
#'
#' @import data.table stringr aou.reader
#' @export
dcm <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  #commented out codes will likely be used in future update

  #icd10s
  dcm_icd10s <- c("I42.0")
  subtype_icd10s <- c("I42.6","I42.7","O90.3")
  #lvsd_icd10s <- c("I50.2","I50.42")
  icd10_codes = c(dcm_icd10s, subtype_icd10s)

  #icd9s
  subtype_icd9s <- c("674.5","425.5")
  #lvsd_icd9s <- c("428.2","428.4")
  icd9_codes = subtype_icd9s

  #snomeds
  dcm_snomeds <- c("52029003","471890009","195021004","53043001","766883006")
  #lvsd_snomeds <- c("153941000119100","417996009","703275009","703272007","703274008","443254009","153931000119109","443253003","441481004","120871000119108","120861000119102","120851000119104","1064941000000100")
  subtype_snomeds <- c("204981000119101","30496006","74249003","111285003","1149109001","16253001","72972005","880042006","1234750000","12563008","62377009","16253001")
  snomed_codes = c(dcm_snomeds, subtype_snomeds)
  
  result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
  result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
  result_snomed <- aou.reader::snomed_query(snomed_codes,anchor_date_table,before,after)
  result_all <- rbindlist(list(result_icd9,result_icd10,result_snomed))
  result_all <- setDT(result_all)[,.(dcm_status = length(condition_start_date) > 0,
                                     dcm_entry_date = min(condition_start_date)),
                                  .(person_id)]
  .write_to_bucket(result_all,output_folder,"dcm")
}