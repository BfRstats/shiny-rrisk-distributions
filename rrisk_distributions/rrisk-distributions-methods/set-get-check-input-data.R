rriskDistributionsClass$set("public",
  "set_data", function(x = NULL, y = NULL, input_type = "pdf")
  {
    #cat("set_data\n")
    
    if (length(trimws(x)) == 0) x <- NULL
    if (length(trimws(y)) == 0) y <- NULL
    
    result <- private$check_input_data(x, y, input_type)
    if (result$is_ok == FALSE)
      return(result)

    # set data
    private$provided_data     <- x
    private$provided_data_cdf <- y
    private$data_type         <- input_type
    
    private$tickle_shiny()
    
    result
  }
)

rriskDistributionsClass$set("public",
  "get_data", function()
  {
    list(provided_data     = private$provided_data,
         provided_data_cdf = private$provided_data_cdf,
         data_type         = private$data_type)
  }
)

#-------------------------------------------------------------------------------

rriskDistributionsClass$set("private",
  "check_input_data", function(input_data, input_data_cdf, input_type)
  {
    result <- list(is_ok = TRUE,
                   error_message = "")
    
    # check general input data
    if (any(is.na(input_data))) {
      result$is_ok <- FALSE
      result$error_message <- "check input data"
    }
    
    # check input type:
    #  "pdf" for probability density function fitting
    #  "cdf" for cumulative density function fitting
    if (!(input_type %in% c("pdf", "cdf"))) {
      result$is_ok <- FALSE
      result$error_message <- paste(result$error_message,
                                    "input type must be either 'pdf' or 'cdf'")
    }
    
    # if input type is "cdf", check input_data_cdf
    if (input_type == "cdf") {
      # check if cdf input is ok
      if (any(is.na(input_data_cdf))) {
        result$is_ok <- FALSE
        result$error_message <- paste(result$error_message,
                                      "check input data cdf")
      } else {
        # check if input_data and input_data_cdf have the same length
        if (length(input_data) != length(input_data_cdf)) {
          result$is_ok <- FALSE
          result$error_message <- paste(result$eror_message,
                                        "input data and input data cdf must be of the same length")
        }
        
        # check if input_data_cdf is in range [0,1]
        if (min(input_data_cdf) < 0 ||
            max(input_data_cdf) > 1) {
          result$is_ok <- FALSE
          result$error_message <- paste(result$error_message,
                                        "input data cdf must be in the range [0,1]")
        }
      }
    }
    
    result
  }
)