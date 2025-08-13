#' Hypertension primary inpatient all
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/mi_primary_inpatient.csv
#' @details Requires at least 1 ICD9/10 code:
#' ICD9:"401","401","401.1","401.9","405","405","405.01",
#'   "405.09","405.1","405.11","405.19","405.9","405.91",
#'   "405.99","362.11","403","403","403","403.01","403.1",
#'   "403.1","403.11","403.9","403.9","403.91","437.2",
#'   "402","402","402.1","402.9","402.1","402.11","402.01",
#'   "402","402.9","402.91","404","404","404.1","404.9",
#'   "404.13","404.93","404.12","404.03","404.02","404.92",
#'   "404.11","404.1","404.01","404","404.91","404.9"
#'  ICD10:
#'    "I10","I16","I16.9","I16.1","I16.0","I1A",
#'    "I1A.0","I15","I15.0","I15.1","I15.2","I15.8",
#'    "I15.9","H35.03","H35.033","H35.032","H35.031",
#'    "H35.039","I12","I12.0","I12.9","O10.22","O10.21",
#'    "O10.23","O10.211","O10.212","O10.213","O10.219",
#'    "O10.2","I67.4","I11","I11.0","I11.9","I13",
#'    "I13.0","I13.10","I13.1","I13.11","I13.2","I13.9",
#'    "O10.1","O10.12","O10.13","O10.111","O10.112",
#'    "O10.113","O10.119","O10.32","O10.31","O10.3",
#'    "O10.33","O10.311","O10.312","O10.313","O10.319"
#' @export
hypertension_primary_inpatient <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  icd9_codes <- c("I10","I16","I16.9","I16.1","I16.0","I1A",
                    "I1A.0","I15","I15.0","I15.1","I15.2","I15.8",
                    "I15.9","H35.03","H35.033","H35.032","H35.031",
                    "H35.039","I12","I12.0","I12.9","O10.22","O10.21",
                    "O10.23","O10.211","O10.212","O10.213","O10.219",
                    "O10.2","I67.4","I11","I11.0","I11.9","I13",
                    "I13.0","I13.10","I13.1","I13.11","I13.2","I13.9",
                    "O10.1","O10.12","O10.13","O10.111","O10.112",
                    "O10.113","O10.119","O10.32","O10.31","O10.3",
                    "O10.33","O10.311","O10.312","O10.313","O10.319")
  icd10_codes <- c("401","401","401.1","401.9","405","405","405.01",
                    "405.09","405.1","405.11","405.19","405.9","405.91",
                    "405.99","362.11","403","403","403","403.01","403.1",
                    "403.1","403.11","403.9","403.9","403.91","437.2",
                    "402","402","402.1","402.9","402.1","402.11","402.01",
                    "402","402.9","402.91","404","404","404.1","404.9",
                    "404.13","404.93","404.12","404.03","404.02","404.92",
                    "404.11","404.1","404.01","404","404.91","404.9")
  icd_codes <- c(icd9_codes,icd10_codes)
  result_all <- aou.reader::hospitalization_query(icd_codes,anchor_date_table,before,after)
  result_all <- result_all[order(hospitalization_entry_date)]
  result_all <- setDT(result_all)[,.(hypertension_primary_inpatient_status = TRUE,
                                     hypertension_primary_inpatient_entry_date = hospitalization_entry_date[1]),
                                  .(person_id)]
  .write_to_bucket(result_all,output_folder,"hypertension_primary_inpatient")
}
