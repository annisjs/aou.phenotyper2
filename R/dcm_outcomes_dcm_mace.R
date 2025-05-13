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
dcm_outcomes_dcm_mace <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c('33.6','37.51','V42.1','785.51','37.66','37.6','398.91','402.01','402.11',
      '402.91','404.01','404.03','404.11','404.13','404.91','404.93','428.0','428.1','428.20','428.21',
      '428.22','428.23','428.30','428.31','428.32','428.33','428.40','428.41','428.42','428.43','428.9',
      '427.1','427.41','427.4','427.5','427.5','798','V12.53','427.42','427.5','798.0','798','798.1',
      '798.2','798.9','V12.53','779.85')

    icd10_codes <- c('Z48.21','02YA0Z0','R57.0','Z95.811','I11.0','I13.0','I13.2','I50','I50.0',
    'I50.1','I50.2','I50.20','I50.21','I50.22','I50.23','I50.3','I50.30','I50.31','I50.32','I50.33',
    'I50.4','I50.40','I50.41','I50.42','I50.43','I50.8','I50.81','I50.810','I50.811','I50.812','I50.813',
    'I50.814','I50.82','I50.83','I50.84','I50.89','I50.9','I97.11','I97.110','I97.111','I97.13',
    'I97.130','I97.131','O29.12','O29.121','O29.122','O29.123','O29.129','P29.0','I47.2','I47.0',
    'I49.02','I49.01','I46.2','I46.8','I48.9','Z86.74','I49.0','I46.9','P29.81','R99','Z84.82','Z86.74')

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    cpt <- c("33945","580","93750","0451T","0452T","0453T","0454T","0455T","0456T","0457T",
    "0458T","0459T","0460T","0461T","33982","33983","33990","92950","92950")

    cpt_dat <- aou.reader::cpt_query(cpt,anchor_date_table,before,after)
    cpt_dat <- cpt_dat[,c("person_id")]
    #make cpt output date match icd output date name
    #setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    final <- rbind(result_all,cpt_dat)
    #cpt_dat <- cpt_dat[!duplicated(pace_cpt_dat)]
    #cpt_dat[, pacemaker_status := TRUE]

    #result_all <- merge(result_all, cpt_dat, by="person_id", all.x=TRUE)

    #result_all[is.na(pacemaker_status), pacemaker_status := FALSE]
    #result_all <- result_all[pacemaker_status == FALSE]
    final_write <- final[,.(dcm_outcomes_dcm_mace_entry_date = min(condition_start_date),
                                        dcm_outcomes_dcm_mace_status = length(condition_start_date) >= 1),
                                    .(person_id)]
    .write_to_bucket(final_write,output_folder,"dcm_outcomes_dcm_mace")
}