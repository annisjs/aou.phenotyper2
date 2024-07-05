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
afib_strict <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd9_codes <- c("427.3","427.31","427.32")
    icd10_codes <- c("I48","I48.0","I48.1","I48.2","I48.3","I48.4","I48.9","I48.91","I48.92")
    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)
    # pacemaker
    pace_cpt <- c("0387T","0389T","0390T","0391T","33207","33208","33210","33211","33212","33213","33214","33221","33222","33226","33227",
                "33228","33229","33233","33234","33235","33236","33237","33238","33274","33275",
                "71090","93279","93280","93281","93286","93288","93293","93294","93731","93732","93733","93734","93735","93736")
    pace_cpt_dat <- aou.reader::cpt_query(pace_cpt,anchor_date_table,before,after)
    pace_cpt_dat <- pace_cpt_dat[,c("person_id")]
    pace_cpt_dat <- pace_cpt_dat[!duplicated(pace_cpt_dat)]
    pace_cpt_dat[, pacemaker_status := TRUE]
    result_all <- merge(result_all, pace_cpt_dat, by="person_id", all.x=TRUE)
    result_all[is.na(pacemaker_status), pacemaker_status := FALSE]
    result_all <- result_all[pacemaker_status == FALSE]
    result_all <- result_all[,.(afib_strict_entry_date = min(condition_start_date),
                                        afib_strict_status = length(unique(condition_start_date)) >= 2),
                                    .(person_id)]
    .write_to_bucket(result_all,output_folder,"afib_strict")
}
