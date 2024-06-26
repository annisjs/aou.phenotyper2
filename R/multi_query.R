#' Multiple Queries
#'
#' @param query_list a list of names of query functions to run
#' @param output_folder the folder to write the output
#' @param bucket the bucket to check for cached query
#' @param load_queries either to load results of the queries into the environment. If set true function will read each resulting query csv file and return the result. the names in list are the same as query_list
#' @param override either to override previous results
#' @param merge_queries either to merge queries. This uses a full merge to maintain all query information therefore might cause a cartesian product. If load_queries is set to false this parameter is ignored.
#' @param merge_on a column name to merge queries on. If load_queries or merge_queries is set to false this parameter is ignored.
#' @param ... optional parameter to be passed to the query. If a query gets an unexpected parameter this may raise an error.
#' @details Runs multiple queries given by a list of function names. 
#' Name in the function needs to be exactly same with query functions in aou.phenotyper2 package.
#'
#' 
#' @return query results saved as output_folder/query_list[[i]].csv. 
#' If load_queries is set to true and merge_queries is false a named list with each query result is returned
#' If merge_queries is not TRUE the query results are merged on the given column name and the resulting data table is returned.
#' @export
multi_query <- function(query_list, output_folder="datasets", bucket=NULL, load_queries = FALSE, override = FALSE, merge_queries=FALSE, merge_on = "person_id", ...)
{
  missing_functions <- query_list[!query_list %in% ls("package:aou.phenotyper2")]
  for (missing_function in missing_functions){
    cat(paste0("The function name ", missing_function, " cannot be found in aou.phenotyper2 queries. Please check the entries.\n"))
  }
  funcs_to_run <- query_list[query_list %in% ls("package:aou.phenotyper2")]
  if(length(funcs_to_run) == 0){
    cat("No valid function can be found in the list. No queries applied!")
  }else{
    query_results <- lapply(funcs_to_run, run_query, output_folder=output_folder, bucket=bucket, load_query=load_queries, override=override, ...)
    names(query_results) <- funcs_to_run
    if(load_queries && !merge_queries){
      return(query_results)
    }else if(merge_queries){
      if(!all(lapply(query_results, function(x){merge_on %in% colnames(x)}))){
        cat("Some of the queries doesn't have the columns to merge on. Returning query list instead.")
        return(query_results)
      }else{
        return(Reduce(function(x,y) merge(x,y,by=merge_on, all.x=T, all.y=T), query_results))
      }
    }
  }
}