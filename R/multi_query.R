#' Multiple Queries
#'
#' @param query_list a list of names of query functions to run
#' @param output_folder the folder to write the output
#' @param load_queries either to load results of the queries into the environment. If set true function will read each resulting query csv file and return the result. the names in list are the same as query_list
#' @param override either to override previous results
#' @param merge_queries either to merge queries. This uses a full merge to maintain all query information therefore might cause a cartesian product. If load_queries is set to false this parameter is ignored.
#' @param merge_on a column name to merge queries on. If load_queries or merge_queries is set to false this parameter is ignored.
#' @param anchor_date_table a data.frame containing two columns: person_id, anchor_date. A time window can be defined around the anchor date using the \code{before} and \code{after} arguments.
#' @param before an integer greater than or equal to 0. Dates prior to anchor_date + before will be excluded.
#' @param after an integer greater than or equal to 0. Dates after anchor_date + after will be excluded.
#' @details Runs multiple queries given by a list of function names. 
#' Name in the function needs to be exactly same with query functions in aou.phenotyper2 package.
#'
#' 
#' @return query results saved as output_folder/query_list[[i]].csv. 
#' If load_queries is set to true and merge_queries is false a named list with each query result is returned
#' If merge_queries is not TRUE the query results are merged on the given column name and the resulting data table is returned.
#' @export
multi_query <- function(query_list, output_folder, load_queries = FALSE, override = FALSE, merge_queries=FALSE, merge_on = "person_id", anchor_date_table=NULL,before=NULL,after=NULL)
{
  missing_functions <- query_list[!query_list %in% ls("package:aou.phenotyper2")]
  for (missing_function in missing_functions){
    cat(paste0("The function name ", missing_function, " cannot be found in aou.phenotyper2 queries. Please check the entries.\n"))
  }
  funcs_to_run <- query_list[query_list %in% ls("package:aou.phenotyper2")]
  if(length(funcs_to_run) == 0){
    cat("No valid function can be found in the list. No queries applied!")
  }else{
    for(func_to_run in funcs_to_run){
      if(override){
        cat(paste0("Running ", func_to_run, "\n"))
        match.fun(func_to_run)(output_folder, anchor_date_table=NULL,before=NULL,after=NULL) 
      }else if(paste0(func_to_run, ".csv") %in% lapply(str_split(ls_bucket("datasets"), "/"), function(x) x[[length(x)]])){
        cat(paste0(func_to_run, " query is cached. Skipping...\n"))
      }else{
        cat(paste0("Running ", func_to_run, "\n"))
        match.fun(func_to_run)(output_folder, anchor_date_table=NULL,before=NULL,after=NULL) 
      }
    }
    if(load_queries){
      query_results <- list()
      for(func_to_run in funcs_to_run){
        query_results[[func_to_run]] <- read_bucket(paste0(output_folder,"/",func_to_run,".csv"))
      }
      if(!merge_queries){
        return(query_results) 
      }else{
        if(!all(lapply(query_results, function(x){merge_on %in% colnames(x)}))){
          cat("Some of the queries doesn't have the columns to merge on. Returning query list instead.")
          return(query_results)
        }else{
          return(Reduce(function(x,y) merge(x,y,by="person_id", all.x=T, all.y=T), query_results))
        }
      }
    }
  }
}
