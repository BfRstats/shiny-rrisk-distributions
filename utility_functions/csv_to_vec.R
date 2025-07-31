# x is a charcter vector, e.g. x = c("1, 2", "3 ")
# final result is a vector, e.g. finaL_result c(1, 2, 3)
csv_to_vec <- function(x, type = "numeric")
{
  if (is.null(x)) return(numeric(0))
  # split result
  split_results <- strsplit(x, ",")
  # concatenation to vector and trim white space of split results
  final_result <- NULL
  for (this_split in split_results) {
    final_result <- c(final_result,
                      trimws(this_split, which = "both"))
  }
  # change character to numeric
  final_result <- if (type == "numeric") as.numeric(final_result)
  # return final results
  final_result
}