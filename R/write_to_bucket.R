#' Internal function used to write dataset to workspace bucket and local file system
#' @param dat the data to write
#' @param output_folder the folder to write to
#' @param name the name of the file to write (without extension)
#' @import stringr
.write_to_bucket <- function(dat,output_folder,name)
{
    dat <- dat[!duplicated(dat)]
    if (missing(output_folder))
    {
        return(dat)
    } else {
        file <- paste0(name,".csv")
        data.table::fwrite(dat,file)
        aou.bucket::cp_to_bucket(file, stringr::str_glue("{output_folder}/{file}"))
    }
}