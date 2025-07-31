rriskDistributionsClass$set("public",
  "plot_hist", function(dist_name = NULL)
  {
    # # if there is no dist_name, or no data, plot an empty plot, and return
    if ( #missing(dist_name) || 
         #isFALSE(nzchar(trimws(dist_name))) ||
         is.null(private$provided_data) ) {
      plot(x    = NULL, 
           y    = NULL, 
           xlim = c(0,1), 
           ylim = c(0,1),
           xlab = "x", 
           ylab = "Density")
      text(x = 0.5, y = 0.5, labels = "no data")
      return(invisible(NULL))
    }
    
    min_x  <- min(private$provided_data)
    max_x  <- max(private$provided_data)
    diff_x <- abs(max_x - min_x)
    min_x  <- min_x - 0.1*diff_x
    max_x  <- max_x + 0.1*diff_x
      
    if (is.na(min_x) || is.na(max_x)) return(invisible(NULL))
    
    if (private$data_type == "pdf") {
      # plot histogram for pdf
      hist(x      = private$provided_data, 
           breaks = 40, 
           freq   = FALSE,
           xlim   = c(min_x, max_x),
           main   = paste(dist_name),
           xlab   = "x")
    } else if (private$data_type == "cdf") {
      # plot histogram for cdf
      diff_x <- diff(private$provided_data)
      diff_y <- diff(private$provided_data_cdf)
      slope <- diff_y / diff_x
      
      is_finite <- is.finite(slope)
      slope <- slope[is_finite]
      diff_x <- diff_x[is_finite]
      
      plot(x    = NULL, 
           y    = NULL, 
           xlim = range(private$provided_data), 
           ylim = c(0, 1.1 * max(slope)),
           xlab = "x",
           ylab = "Density")
      
      n <- length(private$provided_data)-1
      x <- private$provided_data[1:n]
      x <- x[is_finite]
      rect(xleft   = x,
           ybottom = rep(0, length(diff_x)),
           xright  = x + diff_x,
           ytop    = slope,
           col     = "gray")
    }
    
    #---BEGIN: set plot data and stuff------------------------------------------
    
    if ( missing(dist_name) || 
         isFALSE(nzchar(trimws(dist_name)))) dist_name <- "" 
    
    # plot all fit data
    fitted_dist_names <- names(private$fit_results)
    fitted_dist_names <- push_to_end(fitted_dist_names, dist_name)
    for (this_dist_name in fitted_dist_names) { 
      par      <- private$fit_results[[this_dist_name]]$par
      pdf_func <- private$dist_info_list[[this_dist_name]]$pdf_func
      # set plotting function
      plot_pdf <- function(x)
      {
        the_args <- c(list(x = x), as.list(par))
        do.call(pdf_func, args = the_args)
      }
      # plot fitted pdf
      curve(plot_pdf, 
            from = min_x, 
            to   = max_x,
            n    = 201, 
            add  = TRUE,
            lwd  = if (this_dist_name == dist_name) 2 else 1,
            col  = if (this_dist_name == dist_name) "red" else "gray"
      )
    }
    #---END: set plot data and stuff--------------------------------------------
    
    invisible(NULL)
  }
)

rriskDistributionsClass$set("public",
  "plot_ecdf", function(dist_name)
  {
    # if there is no dist_name, or no data, then plot empty plot, and return
    if ( #missing(dist_name) || 
         #isFALSE(nzchar(trimws(dist_name))) ||
         is.null(private$provided_data) ) {
      plot(x    = NULL, 
           y    = NULL, 
           xlim = c(0,1), 
           ylim = c(0,1),
           xlab = "x", 
           ylab = "Fn(x)"
      )
      text(x = 0.5, y = 0.5, labels = "no data")
      return(invisible(NULL))
    }
    
    # there is data
    min_x <- min(private$provided_data)
    max_x <- max(private$provided_data)
    diff_x <- abs(max_x - min_x)
    min_x <- min_x - 0.1*diff_x
    max_x <- max_x + 0.1*diff_x
    
    if (is.na(min_x) || is.na(max_x)) return(invisible(NULL))

    # prepare data for ecdf plot
    if (private$data_type == "pdf") {
      x <- sort(private$provided_data)
      y <- ecdf(x)(x)
    } else if (private$data_type == "cdf") {
      x <- private$provided_data
      y <- private$provided_data_cdf
    }
    
    # plot ecdf data
    plot(x    = x,
         y    = y,
         type = "s",
         xlab = "x",
         ylab = "Fn(x)",
         main = "",
         xlim = c(min_x, max_x),
         ylim = c(0,1),
         lty  = 3)
    lines(x    = x,
          y    = y,
          type = "S",
          lty  = 3)
    points(x    = x,
           y    = y,
           pch  = 19,
           type = "b")
    abline(h = c(0,1), lty = 2)
    #---BEGIN: set plot fit data------------------------------------------------
    
    if ( missing(dist_name) || 
         isFALSE(nzchar(trimws(dist_name)))) dist_name <- ""
    
    fitted_dist_names <- names(private$fit_results)
    fitted_dist_names <- push_to_end(fitted_dist_names, dist_name)

    for (this_dist_name in fitted_dist_names) {
      # get fit data
      par <- private$fit_results[[this_dist_name]]$par
      cdf_func <- private$dist_info_list[[this_dist_name]]$cdf_func
      # set plotting function
      plot_cdf <- function(x)
      {
        the_args <- c(list(q = x), as.list(par))
        do.call(cdf_func, args = the_args)
      }
      # plot fitted cdf
      curve(plot_cdf,
            from = min_x, 
            to   = max_x,
            n    = 201,
            add  = TRUE,
            lwd  = if (this_dist_name == dist_name) 2 else 1,
            col  = if (this_dist_name == dist_name) "red" else "gray"
      )
    }
    #---END: set plot fit data--------------------------------------------------
    
    invisible(NULL)
  }
)