#' Bacterial infection
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/bacterial_infection.csv
#' @details At least 1 ICD 10 code (ICD 9 is not included):
#'
#' Pneumonia/respiratory
#' J18.9: Unspecified Pneumonia
#' J15.0: Pneumonia due to Klebsiella pneumoniae
#' J15.1: Pneumonia due to Pseudomonas
#' J15.2: Pneumonia due to Staphylococcus
#' J15.3: Pneumonia due to Streptococcus group B
#' J15.4: Pneumonia due to other streptococci
#' J15.6: Pneumonia due to other Gram-negative bacteria
#' J15.7: Pneumonia due to Mycoplasma pneumoniae
#' J15.8: Pneumonia due to other specified bacteria
#' J18.0: Bronchopneumonia, unspecified organism
#' J18.8: Other pneumonia, unspecified organism
#' J86.0: Empyema
#' J85: Lung abscess
#' J01.0: Acute maxillary sinusitis
#' J01.1: Acute frontal sinusitis
#' J01.2: Acute ethmoidal sinusitis
#' J01.3: Acute sphenoidal sinusitis
#' J01.4: Acute pansinusitis
#' J01.8: Other acute sinusitis
#' J01.9: Acute sinusitis, unspecified
#' 
#' Genitourinary infections
#' N39.0: Urinary tract infection, site not specified
#' N39.1: Cystitis (bladder infection)
#' N39.2: Pyelonephritis (kidney infection)
#' A54.0: Gonorrhea
#' A56.0: Chlamydia
#' A54.29: Other gonococcal genitourinary infections
#' A56.19: Other chlamydial genitourinary infections
#' 
#' Skin and soft tissue infections
#' L00-L02: Abscesses and furuncles
#' L03: Cellulitis and lymphangitis
#' L06: Necrosis of skin and subcutaneous tissue
#' L07: Myositis
#' L08: Other infections of skin and subcutaneous tissue, unspecified
#' 
#' Bone and joint infections
#' M86.x: Osteomyelitis
#' M00.x: Septic Artiritis
#' T84.50: infection and inflammatory reaction due to an unspecified internal joint prosthesis. 
#' T84.7: infection due to other internal orthopedic prosthetic devices, implants, and grafts.
#' M46.24: Osteomyelitis of vertebra, thoracic region
#' M46.34: Infection of intervertebral disc (pyogenic), thoracic region:
#' 
#' Gastrointestinal infections
#' K52.9: Unspecified infectious gastroenteritis and colitis
#' A00-A01: Cholera
#' A02: Salmonella infections
#' A03: Shigellosis
#' A04: Other bacterial intestinal infections
#' A04.9: Bacterial intestinal infection, unspecified
#' K81.0: Acute cholecystitis
#' K81.2: Acute cholecystitis with chronic cholecystitis
#' K81.9: Cholecystitis, unspecified
#' CNS infections
#' G00.0: Hemophilus influenzae meningitis
#' G00.1: Pneumococcal meningitis
#' G00.2: Streptococcal meningitis
#' G00.3: Staphylococcal meningitis
#' G00.8: Other bacterial meningitis
#' G00.9: Bacterial meningitis, unspecified
#' A39: Meningococcal infections
#' A40: Other bacterial meningitis
#' 
#' @export
bacterial_infection <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    icd10_codes <- c(
    # Pneumonia/respiratory
    "J18.9", "J15.0", "J15.1", "J15.2", "J15.3", "J15.4", "J15.6", "J15.7", "J15.8", 
    "J18.0", "J18.8", "J86.0", "J85", "J01.0", "J01.1", "J01.2", "J01.3", "J01.4", 
    "J01.8", "J01.9",
    
    # Genitourinary infections
    "N39.0", "N39.1", "N39.2", "A54.0", "A56.0", "A54.29", "A56.19",
    
    # Skin and soft tissue infections
    "L00-L02", "L03", "L06", "L07", "L08",
    
    # Bone and joint infections
    "M86.x", "M00.x", "T84.50", "T84.7", "M46.24", "M46.34",
    
    # Gastrointestinal infections
    "K52.9", "A00-A01", "A02", "A03", "A04", "A04.9", "K81.0", "K81.2", "K81.9",
    
    # CNS infections
    "G00.0", "G00.1", "G00.2", "G00.3", "G00.8", "G00.9", "A39", "A40"
    )
    result_icd10 <- aou.reader::icd10_query(icd10_codes,anchor_date_table,before,after)
    result_all <- result_all[order(condition_start_date)]
    result_all <- result_all[,.(bacterial_infection_entry_date = condition_start_date[1],
                                bacterial_infection_status = TRUE),
                            .(person_id)]
    .write_to_bucket(result_all,output_folder,"bacterial_infection")
}