#' Internal function used to read dataset from workspace bucket and local file system
#' @param dat the data to read
#' @param output_folder the folder to read from
#' @param name the name of the file to read (without extension)
#' @import stringr
.read_from_bucket <- function(output_folder,name)
{
  file <- paste0(name,".csv")
  print(file)
  print("output_folder}/{file}")
  returned_data <- aou.bucket::cp_from_bucket(file, stringr::str_glue("{output_folder}/{file}"))
  return(returned_data)
}