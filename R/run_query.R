#' Run Query
#'
#' @param query_name a string of query name to run
#' @param output_folder the folder to write the output and check for the cache
#' @param bucket the bucket to check for cached query
#' @param load_query either to load the result
#' @param override either to override previous results
#' @param ... optional parameter to be passed to the query
#' @details Runs a query after checking the cache in the output_folder. 
#' Name in the function needs to be exactly same with query functions in aou.phenotyper2 package.
#'
#' 
#' @return query result saved as output_folder/query_name.csv. If load_query is set to TRUE, query result is also returned.
#' @export
run_query <- function(query_name, output_folder="datasets", bucket = NULL, load_query = FALSE, override = FALSE, ...)
{
  if(!query_name %in% ls("package:aou.phenotyper2")){
    stop("The function name ", query_name, " cannot be found in aou.phenotyper2 queries. Please check the entries.\n")
  }
  if(is.null(bucket)){
    bucket <- Sys.getenv("WORKSPACE_BUCKET")
  }
  if(override || (system(str_glue("gsutil ls {bucket}/{output_folder}/{query_name}.csv"))==1)){
    cat(paste0("Running ", query_name, "\n"))
    match.fun(query_name)(output_folder, ...)
    if(Sys.getenv("WORKSPACE_BUCKET") != bucket){
      system(str_glue('gsutil cp {Sys.getenv("WORKSPACE_BUCKET")}/{output_folder}/{query_name}.csv {bucket}/{output_folder}/{query_name}.csv'))
    }
  }else{
    if(bucket == Sys.getenv("WORKSPACE_BUCKET")){
      cat(paste0(query_name, " query is cached. Skipping...\n"))
    }else{
      cat(str_glue("The {query_name} is cached from {bucket}. Skipping...\n"))
    }
  }
  if(load_query){
    return(read_bucket(paste0(bucket,"/",output_folder,"/",query_name, ".csv")))
  }
}