# internal function
get_proper_file_name <- function(file_name, file_ext)
{
  if (nzchar(trimws(file_name))) {
    # remove "."
    file_name <- gsub("\\.", "", file_name)
    # remove "*"
    file_name <- gsub("\\*", "", file_name)
    # replace white space with underscore
    file_name <- gsub(" ", "_", file_name)
    # add file extension
    file_name <- paste0(file_name, ".", file_ext)
    # return proper file name
  } else {
    # no file_name given; set default file_name
    file_name <- paste0("shiny-rrisk-dist_model.", file_ext)
  }
  file_name
}