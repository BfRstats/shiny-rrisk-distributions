parseActionButton <- function(idstr) 
{
  id <- as.integer(sub(".*_([0-9]+)", "\\1", idstr))
  if (!is.na(id)) id #?
}