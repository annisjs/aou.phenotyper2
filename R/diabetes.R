#' Diabetes
#'
#' @param output_folder the folder to write the output
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Based on the definition found in AOU phenotype library. Uses ICD9/10, meds, labs.
#' @return output_folder/diabetes.csv
#' @import data.table stringr aou.reader
#' @export
diabetes <- function(output_folder,anchor_date_table=NULL,before=NULL,after=NULL)
{
  dataset <- Sys.getenv("WORKSPACE_CDR")
  condition_codes_t1d_icd9 <- c('250.01', '250.11', '250.21', '250.31', '250.41', '250.51', '250.61', '250.71',
                                '250.81', '250.91', '250.03', '250.13', '250.23', '250.33', '250.43', '250.53',
                                '250.63', '250.73', '250.83', '250.93')

  condition_codes_t1d_icd10 <- c('E10.9', 'E10.65', 'E10.10', 'E10.69', 'E10.11', 'E10.641', 'E10.29', 'E10.21',
                                 'E10.36', 'E10.319', 'E10.39', 'E10.311', 'E10.40', 'E10.51', 'E10.649', 'E10.621',
                                 'E10.628', 'E10.630', 'E10.638', 'E10.620', 'E10.622', 'E10.618', 'E10.8')

  drug_names_t1d <- c('insulin', 'pramlintide', 'insulin glulisine', 'insulin lispro', 'insulin aspart',
                      'insulin glargine', 'insulin detemir', 'insulin degludec', 'insulin NPH')

  condition_codes_t2d_icd9 <- c('250.00', '250.02', '250.20', '250.22', '250.30', '250.32', '250.40', '250.42', '250.50',
                                '250.52', '250.60', '250.62', '250.70', '250.72', '250.80', '250.82', '250.90', '250.92')

  condition_codes_t2d_icd10 <- c('E11.9', 'E11.65', 'E11.01', 'E11.00', 'E11.641', 'E11.29', 'E11.21', 'E11.319',
                                 'E11.39', 'E11.36', 'E11.311', 'E11.40', 'E11.51', 'E11.630', 'E11.638', 'E11.620',
                                 'E11.622', 'E11.621', 'E11.618', 'E11.628', 'E11.69', 'E11.649', 'E11.8')

  drug_names_t2d <- c('acetohexamide', 'tolazamide', 'chlorpropamide', 'glipizide', 'glyburide', 'glimepiride',
                      'repaglinide', 'nateglinide', 'metformin', 'rosiglitazone', 'pioglitazone', 'troglitazone',
                      'acarbose', 'miglitol', 'sitagliptin', 'exenatide', 'saxagliptin', 'linagliptin', 'liraglutide',
                      'semaglutide', 'canagliflozin', 'dapagliflozin', 'empagliflozin', 'alogliptin', 'colesevelam',
                      'albiglutide', 'dulaglutide', 'lixisenatide')

  condition_codes_t1d_icd9_string <- toString(sprintf("'%s'",condition_codes_t1d_icd9))
  condition_codes_t1d_icd10_string <- toString(sprintf("'%s'",condition_codes_t1d_icd10))

  #Type 1 Diabetes Condition Concept IDs
  condition_t1d_sql <- str_glue("

    SELECT
        c.concept_name,
        c.concept_code,
        c.concept_id
    FROM
        `{dataset}.concept` c
        JOIN `{dataset}.condition_occurrence` co
            ON c.concept_id = co.condition_source_concept_id
    WHERE
        (vocabulary_id='ICD9CM' AND concept_code IN (%s))
        OR (vocabulary_id='ICD10CM' AND concept_code IN (%s))

    GROUP BY
        c.concept_name,
        c.concept_code,
        c.concept_id
    ")

  query <- sprintf(condition_t1d_sql, condition_codes_t1d_icd9_string, condition_codes_t1d_icd10_string)
  condition_concepts_t1d_df <- download_data(query)

  #Type 1 Diabetes Drug Concept IDs
  drug_names_t1d_subquery <- paste('LOWER(c.concept_name) LIKE "%', drug_names_t1d, sep = '', collapse = '%" OR ')
  drug_names_t1d_subquery <- paste(drug_names_t1d_subquery,'%"', sep = "")

  drug_t1d_sql <- str_glue("

    SELECT
        DISTINCT c2.concept_name,
        c2.concept_code,
        c2.concept_id

    FROM
        `{dataset}.concept` c
        JOIN `{dataset}.concept_ancestor` ca
            ON c.concept_id = ca.ancestor_concept_id
        JOIN `{dataset}.concept` c2
            ON c2.concept_id = ca.descendant_concept_id

    WHERE
        c.concept_class_id = 'Ingredient'
        AND ({drug_names_t1d_subquery})

    ")

  drug_concepts_t1d_df <- download_data(drug_t1d_sql)

  #Type 2 Diabetes Condition Concept IDs
  condition_codes_t2d_icd9_string <- toString(sprintf("'%s'",condition_codes_t2d_icd9))
  condition_codes_t2d_icd10_string <- toString(sprintf("'%s'",condition_codes_t2d_icd10))


  condition_t2d_sql <- str_glue("

    SELECT
        c.concept_name,
        c.concept_code,
        c.concept_id
    FROM
        `{dataset}.concept` c
        JOIN `{dataset}.condition_occurrence` co
            ON c.concept_id = co.condition_source_concept_id
    WHERE
        (vocabulary_id='ICD9CM' AND concept_code IN (%s))
        OR (vocabulary_id='ICD10CM' AND concept_code IN (%s))

    GROUP BY
        c.concept_name,
        c.concept_code,
        c.concept_id

    ")
  query <- sprintf(condition_t2d_sql, condition_codes_t2d_icd9_string, condition_codes_t2d_icd10_string)
  condition_concepts_t2d_df <- download_data(query)

  #Type 2 Diabetes Drug Concept IDs
  drug_names_t2d_subquery <- paste('LOWER(c.concept_name) LIKE "%', drug_names_t2d, sep = '', collapse = '%" OR ')
  drug_names_t2d_subquery <- paste(drug_names_t2d_subquery,'%"', sep = "")

  drug_t2d_sql <- str_glue("

    SELECT
        DISTINCT c2.concept_name,
        c2.concept_code,
        c2.concept_id

    FROM
        `{dataset}.concept` c
        JOIN `{dataset}.concept_ancestor` ca
            ON c.concept_id = ca.ancestor_concept_id
        JOIN `{dataset}.concept` c2
            ON c2.concept_id = ca.descendant_concept_id

    WHERE
        c.concept_class_id = 'Ingredient'
        AND ({drug_names_t2d_subquery})

    ")

  drug_concepts_t2d_df <- download_data(drug_t2d_sql)

  #Select Cohort Participants
  condition_concepts_t2d_string <- paste(condition_concepts_t2d_df$concept_id, collapse = ", ")
  drug_concepts_t2d_df_string <- paste(drug_concepts_t2d_df$concept_id, collapse = ", ")
  condition_concepts_t1d_string <- paste(condition_concepts_t1d_df$concept_id, collapse = ", ")

  #Case 1
  case1_sql <- str_glue("

    SELECT p.person_id,
           MIN(d.drug_exposure_start_date) AS drug_start_date,
           MIN(c.condition_start_date) AS condition_start_date
    FROM `{dataset}.person` p
    INNER JOIN
    `{dataset}.condition_occurrence` c
    ON (p.person_id = c.person_id)
    INNER JOIN
    `{dataset}.drug_exposure` d
    ON (c.person_id = d.person_id)
    WHERE
    c.condition_source_concept_id IN (%s) AND
    d.drug_concept_id IN (%s) AND
    p.person_id NOT IN (SELECT person_id
                      FROM `{dataset}.condition_occurrence`
                      WHERE condition_source_concept_id IN (%s))
    GROUP BY p.person_id

    ")


  query <- sprintf(case1_sql, condition_concepts_t2d_string, drug_concepts_t2d_df_string, condition_concepts_t1d_string)
  case1_cohort <- download_data(query)

  #Case 2
  drug_concepts_t1d_df_string <- paste(drug_concepts_t1d_df$concept_id, collapse = ", ")

  case2_sql <- str_glue("

    SELECT p.person_id,
           MIN(c.condition_start_date) AS condition_start_date
    FROM `{dataset}.person` p
    INNER JOIN
    `{dataset}.condition_occurrence` c
    ON (p.person_id = c.person_id)
    WHERE
    c.condition_source_concept_id IN (%s) AND
    p.person_id IN (
                            WITH T1D AS (
                                        SELECT person_id, drug_exposure_start_date
                                        FROM `{dataset}.drug_exposure`
                                        WHERE drug_concept_id IN (%s)
                                        ),
                            T2D AS (
                                    SELECT person_id, drug_exposure_start_date
                                    FROM `{dataset}.drug_exposure`
                                    WHERE drug_concept_id IN (%s)
                                    )

                            SELECT DISTINCT T1D.person_id
                            FROM T1D INNER JOIN T2D ON T1D.person_id = T2D.person_id
                            WHERE DATE_DIFF(T1D.drug_exposure_start_date,T2D.drug_exposure_start_date,DAY) >= 1
                          )
        AND p.person_id NOT IN (SELECT person_id
                              FROM `{dataset}.condition_occurrence`
                              WHERE condition_source_concept_id IN (%s))
    GROUP BY p.person_id

    ")



  query <- sprintf(case2_sql, condition_concepts_t2d_string, drug_concepts_t1d_df_string, drug_concepts_t2d_df_string, condition_concepts_t1d_string)
  case2_cohort <- download_data(query)

  #Case 3
  case3_query <- str_glue("
    WITH tb1 AS (SELECT person_id, MIN(condition_start_date) AS condition_start_date
                      FROM `{dataset}.condition_occurrence`
                      WHERE condition_source_concept_id IN (%s)
                      GROUP BY person_id
                      HAVING COUNT(person_id) >= 2)
    SELECT p.person_id, MIN(t.condition_start_date) AS condition_start_date
    FROM `{dataset}.person` p
    INNER JOIN
    tb1 t
    ON (p.person_id = t.person_id)
    WHERE
        p.person_id IN (SELECT person_id
                          FROM `{dataset}.drug_exposure`
                          WHERE drug_concept_id IN (%s))
        AND p.person_id NOT IN (SELECT person_id
                              FROM `{dataset}.condition_occurrence`
                              WHERE condition_source_concept_id IN (%s))
    GROUP BY p.person_id
    ")

  query <- sprintf(case3_query, condition_concepts_t2d_string, drug_concepts_t1d_df_string, condition_concepts_t1d_string)
  case3_cohort <- download_data(query)

  #Case 4
  case4_query <- str_glue("

        WITH Drug_PIDs AS (
            SELECT person_id, MIN(drug_exposure_start_date) AS drug_exposure_start_date
            FROM `{dataset}.drug_exposure`
            WHERE drug_concept_id IN (%s)
            GROUP BY person_id
        ),

        Lab_PIDs AS (
            SELECT DISTINCT person_id
            FROM `{dataset}.measurement`
            WHERE
                (measurement_source_value IN ('2339-0', '2345-7') AND value_as_number > 200)
                OR (measurement_source_value = '1558-6' AND value_as_number >= 125)
                OR (measurement_source_value IN ('4548-4', '17856-6', '4549-2', '17855-8') AND value_as_number >= 6.5)
        )

        SELECT p.person_id, drug_exposure_start_date
        FROM `{dataset}.person` p
            INNER JOIN Drug_PIDs ON p.person_id = Drug_PIDs.person_id
            INNER JOIN Lab_PIDs ON p.person_id = Lab_PIDs.person_id
        WHERE
            p.person_id NOT IN (SELECT person_id
                                FROM `{dataset}.condition_occurrence`
                                WHERE condition_source_concept_id IN (%s))

        ")

  query <- sprintf(case4_query,drug_concepts_t2d_df_string, condition_concepts_t1d_string)
  case4_cohort <- download_data(query)

  merged_cases <- merge(case1_cohort,case2_cohort,by="person_id",all.x=TRUE,all.y=TRUE)
  merged_cases <- merge(merged_cases,case3_cohort,by="person_id",all.x=TRUE,all.y=TRUE)
  merged_cases <- merge(merged_cases,case4_cohort,by="person_id",all.x=TRUE,all.y=TRUE)
  merged_cases$min_diabetes_date <- apply(merged_cases[,2:ncol(merged_cases)],1,function(x) min(x,na.rm=T))

  all_diabetes <- merged_cases[,c("person_id","min_diabetes_date")]
  setDT(all_diabetes)[,baseline_diabetes_flag := ifelse(!is.na(min_diabetes_date),TRUE,FALSE)]

  colnames(all_diabetes) <- c("person_id","diabetes_entry_date","diabetes_status")
  if (!is.null(anchor_date_table))
  {
    all_diabetes <- as.data.table(merge(all_diabetes,anchor_date_table,by="person_id"))
    all_diabetes[,min_window_date := anchor_date + before]
    all_diabetes[,max_window_date := anchor_date + after]
    all_diabetes <- all_diabetes[diabetes_entry_date >= min_window_date]
    all_diabetes <- all_diabetes[diabetes_entry_date <= max_window_date]
  }
  all_diabetes <- all_diabetes[,c("person_id","diabetes_entry_date","diabetes_status")]
  .write_to_bucket(all_diabetes,output_folder,"diabetes")
}
