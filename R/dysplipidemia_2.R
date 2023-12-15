#' Dyslipidemia
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/dyslipidemia.csv
#' @details Warning! This algorithm will write diabetes.csv, statins.csv to the local disk and may overwrite an existing file.
#' It will not overwrite files on the workspace bucket.
#' Definition:
#' Meets at least one condition:
#'
#' If male and HDL < 40
#'
#' If female and HDL < 50
#'
#' LDL > 160
#'
#' Triglycerides > 150
#'
#' Cholesterol > 240
#'
#' Statin use unless DM
#' @export
dyslipidemia_2 <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
    hdl <- aou.reader::lab_query("Cholesterol in HDL [Mass/volume] in Serum or Plasma",
                                        anchor_date_table,before,after)
    colnames(hdl) <- c("person_id","hdl_date","hdl_value")
    ldl <- aou.reader::lab_query(c("Cholesterol in LDL [Mass/volume] in Serum or Plasma by calculation",
                                "Cholesterol in LDL [Mass/volume] in Serum or Plasma",
                                "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Direct assay",
                                "Cholesterol in LDL [Mass/volume] in Serum or Plasma by Electrophoresis"),
                                        anchor_date_table,before,after)
    colnames(ldl) <- c("person_id","ldl_date","ldl_value")
    trigs <- aou.reader::lab_query(c("Triglyceride [Mass/volume] in Serum or Plasma",
                                "Triglyceride [Mass/volume] in Blood"),
                                        anchor_date_table,before,after)
    colnames(trigs) <- c("person_id","trigs_date","trigs_value")
    chol <- aou.reader::lab_query(c("Cholesterol [Mass/volume] in Serum or Plasma"),
                                        anchor_date_table,before,after)
    colnames(chol) <- c("person_id","chol_date","chol_value")

    dm <- diabetes(anchor_date_table=anchor_date_table,before=before,after=after)
    statins <- statins(anchor_date_table=anchor_date_table,before=before,after=after)

    #HDL
    dem <- demographics()
    dem[,sex := recode_sex_fm(sex)]
    dem <- dem[!is.na(sex)]
    dem <- dem[,c("person_id","sex")]
    hdl_merged <- merge(hdl,dem,by="person_id",all.x=TRUE)
    hdl_merged[,hdl_status := ifelse(sex == "Male",
                                    hdl_value < 40,
                                    ifelse(sex == "Female",
                                            hdl_value < 50,NA))]
    hdl_merged <- hdl_merged[hdl_status == TRUE]
    hdl_merged <- hdl_merged[order(hdl_date,decreasing = FALSE)]
    hdl_agg <- hdl_merged[,.(hdl_status = hdl_status[1],
                            hdl_date = hdl_date[1]),.(person_id)]
    #LDL
    ldl[,ldl_status := ldl_value > 160]
    ldl <- ldl[ldl_status == TRUE]
    ldl <- ldl[order(ldl_date,decreasing = FALSE)]
    ldl_agg <- ldl[,.(ldl_status = ldl_status[1],
                        ldl_date = ldl_date[1]),.(person_id)]
    #Trigs
    trigs[,trigs_status := trigs_value > 150]
    trigs <- trigs[trigs_status == TRUE]
    trigs <- trigs[order(trigs_date,decreasing = FALSE)]
    trigs_agg <- trigs[,.(trigs_status = trigs_status[1],
                            trigs_date = trigs_date[1]),.(person_id)]
    #Chol
    chol[,chol_status := chol_value > 240]
    chol <- chol[chol_status == TRUE]
    chol <- chol[order(chol_date,decreasing = FALSE)]
    chol_agg <- chol[,.(chol_status = chol_status[1],
                        chol_date = chol_date[1]),.(person_id)]

    #Statins
    statins[,statin_status := !is.na(statins_entry_date)]
    statins <- statins[statin_status == TRUE]

    #Merge
    result_all <- merge(hdl_agg,ldl_agg,by="person_id",all.x=T,all.y=T)
    result_all <- merge(result_all,trigs_agg,by="person_id",all.x=T,all.y=T)
    result_all <- merge(result_all,chol_agg,by="person_id",all.x=T,all.y=T)
    result_all <- merge(result_all,statins,by="person_id",all.x=T,all.y=T)
    result_all <- merge(result_all,dm,by="person_id",all.x=T)
    result_all[,diabetes_status := ifelse(is.na(diabetes_status),FALSE,diabetes_status)]
    result_all[,dyslipidemia_status := ifelse(diabetes_status==FALSE,
                                                hdl_status | ldl_status | trigs_status | chol_status | statin_status,
                                                hdl_status | ldl_status | trigs_status | chol_status)]
    result_all[,dyslipidemia_status := ifelse(is.na(dyslipidemia_status),FALSE,dyslipidemia_status)]
    result_all[,dyslipidemia_entry_date := pmin(hdl_date,ldl_date,trigs_date,chol_date,na.rm=T)]
    result_all <- result_all[order(dyslipidemia_entry_date,decreasing=T)]
    result_all <- result_all[,.(dyslipidemia_2_status = dyslipidemia_status[1],
                                dyslipidemia_2_entry_date = dyslipidemia_entry_date[1]),
                            .(person_id)]
    result_all <- result_all[dyslipidemia_2_status == TRUE]
    .write_to_bucket(result_all,output_folder,"dyslipidemia_2")
}

