#' Afib Strict Definition
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/afib_strict.csv
#' @details At least 2 ICD code:
#'
#' ICD9: "427.3","427.31","427.32"
#'
#' ICD10: "I48","I48.0","I48.1","I48.2","I48.3","I48.4","I48.9","I48.91","I48.92"
#'
#' AND No pacemaker defined by at least 1 CPT code:
#' "0387T","0389T","0390T","0391T","33207","33208","33210","33211","33212","33213","33214","33221","33222","33226","33227",
#' "33228","33229","33233","33234","33235","33236","33237","33238","33274","33275",
#' "71090","93279","93280","93281","93286","93288","93293","93294","93731","93732","93733","93734","93735","93736"
#' @export
dcm_outcomes_suddent_cardiac_death <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c('427.5','798.0','798','798.1','798.2','798.9')

    icd10_codes <- c('I46.9','P29.81','R99','V12.53','Z84.82','Z86.74','779.85')

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    cpt <- c('92950')

    cpt_dat <- aou.reader::cpt_query(cpt,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    final <- rbind(result_all,cpt_dat)

    final_write <- final[,.(dcm_outcomes_suddent_cardiac_death_entry_date = min(condition_start_date),
                                        dcm_outcomes_suddent_cardiac_death_status = length(condition_start_date) >= 1),
                                    .(person_id)]
    .write_to_bucket(final_write,output_folder,"dcm_outcomes_sudden_cardiac_death")
}