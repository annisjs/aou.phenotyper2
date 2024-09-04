#' All MI codes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/all_mi_codes.csv
#' ICD9: "410.00","410.01","410.02","410.10","410.11","410.12","410.20","410.21","410.22","410.30","410.31","410.32","410.40","410.41","410.42","410.50","410.51","410.52","410.80","410.81","410.82","410.90","410.91","410.92","411.0","412","429.79"
#'
#' ICD10: "I21", "I21.0","I21.01","I21.02","I21.09","I21.1","I21.11","I21.19","I21.2","I21.21","I21.29","I21.3","I21.4","I21.9","I21.A","I21.A1","I21.A9","I21.B","I22","I22.0","I22.1","I22.2","I22.8","I22.9","I23","I23.0","I23.1","I23.2","I23.3","I23.4","I23.5","I23.6","I23.8","I24.1","I25.2"
#' @export
all_mi_codes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("410.00","410.01","410.02","410.10","410.11","410.12","410.20","410.21","410.22","410.30","410.31","410.32","410.40","410.41","410.42","410.50","410.51","410.52","410.80","410.81","410.82","410.90","410.91","410.92","411.0","412","429.79")
    icd10_codes <- c("I21", "I21.0","I21.01","I21.02","I21.09","I21.1","I21.11","I21.19","I21.2","I21.21","I21.29","I21.3","I21.4","I21.9","I21.A","I21.A1","I21.A9","I21.B","I22","I22.0","I22.1","I22.2","I22.8","I22.9","I23","I23.0","I23.1","I23.2","I23.3","I23.4","I23.5","I23.6","I23.8","I24.1","I25.2")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    colnames(result_all) <- c("person_id","all_mi_codes_entry_date","all_mi_codes_value")
    .write_to_bucket(result_all,output_folder,"all_mi_codes")
}