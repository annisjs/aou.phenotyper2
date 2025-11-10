#' SDOH Survey: Food insecurity
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/sdoh_survey_food_insecurity.csv
#' @details The following 2 questions measure whether or not a participant's household was unable to consistently have food present at home when needed:
#' 
#' 40192426: Were you worried whether the food you had bought just didn't last and you didn't have money to get more?
#' 40192517: Were you worried whether your food would run out before you got money to buy more?
#' Refs: Erin R. Hager, Anna M. Quigg, Maureen M. Black, Sharon M. Coleman, Timothy Heeren, Ruth Rose-Jacobs, John T. Cook, Stephanie A. Ettinger de Cuba, Patrick H. Casey, Mariana Chilton, Diana B. Cutts, Alan F. Meyers, Deborah A. Frank; Development and Validity of a 2-Item Screen to Identify Families at Risk for Food Insecurity. Pediatrics July 2010; 126 (1): e26–e32. 10.1542/peds.2009-3146
#' 
#' Radandt NE, Corbridge T, Johnson DB, Kim AS, Scott JM, Coldwell SE. Validation of a Two-Item Food Security Screening Tool in a Dental Setting. J Dent Child (Chic). 2018 Sep 15;85(3):114-119. PMID: 30869587; PMCID: PMC6419517.#' 
#' @export
sdoh_survey_food_insecurity <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  result <- aou.reader::survey_query(c(40192517, 40192426))
  result[, item_score := fcase(survey_response == "Often true", 1,
                survey_response == "Sometimes true", 1,
                survey_response == "Never true", 0,
                default = NA)]
  result_agg <- result[, .(sdoh_survey_food_insecurity_score = ifelse(any(!is.na(item_score)), 
                                                                ifelse(sum(item_score, na.rm = T) >= 1, 
                                                                     "Food Insecure", 
                                                                     "Not Food Insecure"),
                                                                as.character(NA))),
                      .(person_id)]
  .write_to_bucket(result_agg, output_folder, "sdoh_survey_food_insecurity")
}