#' All Multiple Queries
#'
#' @param query_list a list of names of query functions to run
#' @param output_folder the folder to write the output
#' @param either to load results of the queries into the enviroment. If set true function will read each resulting query csv file and save it to a named list where the names are the same as query_list
#' @param either to override previous results
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Runs multiple queries given by a list of function names. Name in the function needs to be exactly same with query functions in phenotyper2 package.
#'
#' 
#' @return query results saved as output_folder/query_list\[\[i\]\].csv. If load_queries is set to true each query result is loaded to a named list
#' @export
multi_query <- function(query_list, output_folder, load_queries = FALSE, override = FALSE, anchor_date_table=NULL,before=NULL,after=NULL)
{
  missing_functions = query_list[!query_list %in% ls("package:phenotyper2")]
  for (missing_function in missing_functions){
    cat(paste0("The function name ", missing_function, " cannot be found in aou.phenotyper2 queries. Please check the entries.\n"))
  }
  funcs_to_run = query_list[query_list %in% ls("package:aou.phenotyper2")]
  if(length(funcs_to_run) == 0){
    cat("No valid function can be found in the list. No queries applied!")
  }else{
    for(func_to_run in funcs_to_run){
      if(override){
        match.fun(func_to_run)(output_folder, anchor_date_table=NULL,before=NULL,after=NULL) 
      }else if(paste0(func_to_run, ".csv") %in% lapply(str_split(ls_bucket("datasets"), "/"), function(x) x[[length(x)]])){
        cat(paste0(func_to_run, " query is cached. Skipping...\n"))
      }else{
        match.fun(func_to_run)(output_folder, anchor_date_table=NULL,before=NULL,after=NULL) 
      }
    }
    if(load_queries){
      query_results = list()
      for(func_to_run in funcs_to_run){
        query_results[[func_to_run]] = read_bucket(paste0(output_folder,x,".csv"))
      }
      return(query_results)
    }
  }
}
