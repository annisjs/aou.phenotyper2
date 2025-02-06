#' Covid
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @return output_folder/covid.csv
#' @details Uses concept code U09.0:
#'
#' @export
covid <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  covid_test_concept_ids <- c(586520,586523,586525,586526,586529,706157,706159,715261,715272,723470,723472,757678,36032061,36032174,36032258,36661371,586518,586524,706154,706175,723464,723467,723478,36031453,586516,706158,706160,706163,706171,706172,715260,723469,36031213,36661377,586528,706161,706165,706167,723463,723468,723471,757677,36031238,36031944,586519,706166,706169,706173,723465,723476,757685,36031506,706155,706156,706170,723466,36031652,36661370,706168,706174,715262,723477,36032419,36661378,37310257)
  
  positive_test_concept_ids <- c(9191,4126681,36032716,36715206,45878745,45881802,45877985,45884084)
  covid_concept_id = c(37311061)
  
  covid_conditions <- aou.reader::condition_query(concept_id=covid_concept_id,anchor_date_table=anchor_date_table,before=before,after=after)
  covid_conditions <- covid_conditions[,.(covid_entry_date = min(condition_start_date),
                                          covid_status = length(condition_start_date) > 0), .(person_id)]
  
  covid_test_results <- aou.reader::lab_concept_query(lab_concepts=covid_test_concept_ids,anchor_date_table=anchor_date_table,before=before,after=after)
  covid_test_results <- covid_test_results[value_as_concept_id %in% positive_test_concept_ids]
  covid_test_results = covid_test_results[,.(covid_entry_date = min(measurement_date),
                                             covid_status = length(measurement_date) > 0), .(person_id)]
  
  result_all <- rbind(covid_conditions, covid_test_results)
  results_all <- result_all[,.(covid_entry_date = min(covid_entry_date),
                               covid_status = length(covid_entry_date) > 0), .(person_id)]
  result_all = result_all[!duplicated(person_id)]
  .write_to_bucket(result_all,output_folder,"covid")
}
