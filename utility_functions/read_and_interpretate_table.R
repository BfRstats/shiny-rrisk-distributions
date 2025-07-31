convert_to_df <- function(input_data, header = TRUE, split_rule = " ",
                          has_rownames = FALSE)
{
  if (is.null(input_data)) return()
  
  # split data and fill a matrix
  df <- NULL
  for (i in seq_along(input_data)) {
    splitted_row <- strsplit(x = input_data[i], split = split_rule)[[1]]
    if (!(header && i == 1) && has_rownames && length(splitted_row) > 1) {
      splitted_row <- splitted_row[-1]
    }
    df <- suppressWarnings(rbind(df, splitted_row))
  }
  
  # convert to data frame with header
  if (header) {
    n <- nrow(df)
    df_final <- as.data.frame(df[2:n,])
    colnames(df_final) <- df[1,]
  } else {
    df_final <- as.data.frame(df)
  }
  rownames(df_final) <- NULL

  # try to convert columns in to numeric
  for (i in seq_len(ncol(df_final))) {
    tmp <- tryCatch(as.numeric(df_final[,i]),
                    error   = function(e) "error",
                    warning = function(w) "error")
    if (is.numeric(tmp)) {
      df_final[,i] <- tmp
    }
  }
  
  # return result
  df_final
}