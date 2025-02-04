#' Internal function used to write dataset to workspace bucket and local file system
#' @param dat the data to write
#' @param output_folder the folder to write to
#' @param name the name of the file to write (without extension)
#' @param keep_shards if TRUE, original shards from bigquery will be kept and moved to folder under designated name
#' @param query_folder_origin if keep_shards is TRUE, must specify the name of the folder where the query is written to
#' @import stringr
.write_to_bucket <- function(dat,output_folder,name,keep_shards=FALSE,query_folder_origin=NULL)
{
    dat <- dat[!duplicated(dat)]
    if (missing(output_folder))
    {
        return(dat)
    } else {
        if (keep_shards)
        {
            my_bucket <- Sys.getenv("WORKSPACE_BUCKET")
            if(stringr::str_glue("{name}.csv") %in% basename(system(stringr::str_glue("gsutil ls {my_bucket}/{output_folder}"), intern = T)))
            {
                system(stringr::str_glue("gsutil rm -rf {output_folder}/{name}.csv"), intern = T)
            }
            system(stringr::str_glue("gsutil mv {my_bucket}/{query_folder_origin}/* {output_folder}/{name}.csv"),intern=T)
        } else {
            file <- paste0(name,".csv")
            data.table::fwrite(dat,file)
            aou.bucket::cp_to_bucket(file, stringr::str_glue("{output_folder}/{file}"))
        }
    }
}