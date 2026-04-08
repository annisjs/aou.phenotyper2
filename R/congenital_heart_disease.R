#' congenitcal heart disease Definition
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/hcm.csv
#' @export
congenital_heart_disease <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{    
    icd9_codes <- c("V13.65", "746.9", "746.89", "746.86", "746.82", "746.4", "746", "746.3",
    "746.09", "745.5", "745.4", "745.2", "745.11", "745.1", "745.12", "745.19",
    "745.6", "745.69", "745.8", "746.01", "746.02", "746.1", "746.2", "746.5",
    "746.6", "746.81", "746.83", "747.1", "747.31", "747.39", "747.41",
    "747.42", "747.49")

    icd10_codes <- c("Z87.74", "Z87.7", "Q26.9", "Q26.8", "Q26.6", "Q26.5", "Q26.4", "Q26.3",
    "Q26.2", "Q26.1", "Q26.0", "Q26", "Q25.9", "Q25.8", "Q25.79", "Q25.72",
    "Q25.71", "Q25.7", "Q25.6", "Q25.5", "Q25.49", "Q25.42", "Q25.41",
    "Q25.40", "Q25.4", "Q25.3", "Q25.29", "Q25.21", "Q25.2", "Q25.1",
    "Q25.0", "Q25", "Q24.9", "Q24.8", "Q24.6", "Q24.5", "Q24.4", "Q24.3",
    "Q24.2", "Q24.0", "Q24", "Q23.9", "Q23.8", "Q23.4", "Q23.3", "Q23.2",
    "Q23.1", "Q23.0", "Q23", "Q22.9", "Q22.8", "Q22.6", "Q22.5", "Q22.4",
    "Q22.3", "Q22.2", "Q22.1", "Q22.0", "Q22", "Q21.9", "Q21.8", "Q21.4",
    "Q21.3", "Q21.23", "Q21.22", "Q21.21", "Q21.20", "Q21.2", "Q21.19",
    "Q21.16", "Q21.15", "Q21.14", "Q21.13", "Q21.11", "Q21.10", "Q21.1",
    "Q21.0", "Q21", "Q20.9", "Q20.8", "Q20.6", "Q20.5", "Q20.4", "Q20.3",
    "Q20.2", "Q20.1", "Q20.0", "Q20", "I27.83")

    result_icd9 <- aou.reader::icd9_query(icd9_codes,anchor_date_table,before,after)
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- rbind(result_icd9,result_icd10)

    cpt_codes <- c("33608", "33610", "33611", "33615", "33617", "33619", "33622", "33641",
    "33647", "33660", "33665", "33670", "33675", "33676", "33677", "33681",
    "33684", "33688", "33692", "33694", "33697", "33710", "33741", "33745",
    "33746", "33770", "33771", "33774", "33775", "33776", "33777", "33778",
    "33779", "33780", "33781", "33782", "33783", "33786", "33820", "33822",
    "33824", "33840", "33845", "33851", "33852", "33853", "33894", "33895",
    "33897", "93580", "93581", "93593", "93594", "93595", "93596", "93597",
    "93598")


    cpt_dat <- aou.reader::cpt_query(cpt_codes,anchor_date_table,before,after)

    #make cpt output date match icd output date name
    setnames(cpt_dat, "entry_date", "condition_start_date")
    setnames(cpt_dat, "cpt_code", "condition_source_value")

    combined <- rbind(result_all,cpt_dat)

    final_write <- combined[,.(congenital_heart_disease_date = min(condition_start_date),
                                    congenital_heart_disease_status = length(condition_start_date) >= 1),
                                .(person_id)]

  
    final <- final_write[, c("person_id", "congenital_heart_disease_status","congenital_heart_disease_date")]

    .write_to_bucket(final,output_folder,"congenital_heart_disease")
}