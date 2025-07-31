rriskDistributionsClass$set("public",
  "fit_all_distributions", function(silent = FALSE)
  {
    # check readiness of function
    if (is.null(private$provided_data)) return()
    
    # clear current fit result list
    private$fit_results <- list() 
    # fit all applicable functions
    for (dist_name in self$get_applicable_dists())
      self$fit_distribution(dist_name, silent = silent)
    
    invisible(NULL)
  }
)

rriskDistributionsClass$set("public",
  "fit_distribution", function(dist_name, silent = FALSE)
  {
    result <- private$check_input_for_fitting(dist_name)
    if (!result$is_ok)
      if (silent)
        return(result)
      else
        stop(result$error_message)
    
    # try to fit data to distribution dist_name
    fit <- tryCatch(
      private$main_fit_routine(dist_name, 
                               private$dist_info_list[[dist_name]], 
                               list(x = private$provided_data,
                                    y = private$provided_data_cdf),
                               private$data_type),
      error   = function(e) e,
      warning = function(w) w
    )
    
    if (any(c("error", "warning") %in% attributes(fit)$class) ||
        fit$convergence != 0) {
      result$is_ok <- FALSE
      result$error_message <- fit$message
    } else {
      # add successful fit to fit results
      private$fit_results[[dist_name]] <- fit
      # tell shiny something changed
      private$tickle_shiny()
      # if call is not silent return the fit result
      if (!silent) result$fit_result <- private$fit_results[[dist_name]]
    }
    
    result
  }
)

rriskDistributionsClass$set("public",
  "get_fit_result", function(dist_name = NULL, stat_name = "loglike")
  {
    # check dist name and stat_name
    
    # get the fit results
    if (is.null(dist_name)) {
      # sort fit result
      #stat_vec <- get_stat_vec(names(private$fit_results), stat_name)
      stat_vec <- sapply(
        X   = names(private$fit_results),
        FUN = function(dist_name, stat_name)
        {
          switch(
            stat_name,
            loglike = private$fit_results[[dist_name]]$mean_neg_loglike,
            aic     = private$fit_results[[dist_name]]$aic,
            bic     = private$fit_results[[dist_name]]$bic
          )
        },
        stat_name
      )
      private$fit_results[order(stat_vec, decreasing = FALSE)]
    } else
      private$fit_results[[dist_name]]
  }
)

rriskDistributionsClass$set("public",
  "get_fitted_dist_name_list", function(sorted_by = NULL)
  {
    # get names of fitted dist results
    fitted_dist_names <- names(private$fit_results)
    # get display names of fitted dist results
    fitted_dist_display_names <- sapply(
      X   = fitted_dist_names,
      FUN = function(this_dist_name) 
        private$dist_info_list[[this_dist_name]]$display_name,
      simplify  = TRUE,
      USE.NAMES = FALSE
    )    
    # create a named list
    fitted_dist_names_list <- setNames(
      object = as.list(fitted_dist_names),
      nm     = fitted_dist_display_names
    )
    # sort named list of names of fitted dists
    if (!is.null(sorted_by)) {
      #stat_vec <- get_stat_vec(fitted_dist_names, sorted_by)
      stat_vec <- sapply(
        X   = fitted_dist_names,
        FUN = function(dist_name, stat_name)
        {
          switch(
            stat_name,
            loglike = private$fit_results[[dist_name]]$mean_neg_loglike,
            aic     = private$fit_results[[dist_name]]$aic,
            bic     = private$fit_results[[dist_name]]$bic
          )
        },
        sorted_by
      )
      fitted_dist_names_list <- fitted_dist_names_list[order(stat_vec, 
                                                             decreasing = FALSE)]
    }
    # return named list of names of fitted dists
    fitted_dist_names_list
  }
)
#---BEGIN: private methods for density distribution fitting---------------------
rriskDistributionsClass$set("private",
  "main_fit_routine", function(dist_name, dist_info, df, data_type)
  {
    # prepare fit
    if (data_type == "pdf") {
      fit_func  <- private$fit_pdf
      dist_func <- dist_info$pdf_func
    } else if (data_type == "cdf") {
      fit_func  <- private$fit_cdf
      dist_func <- dist_info$cdf_func
    } else {
      stop("ERROR in main_fit_func: data_type unknown")
    }
    # set start params
    par <- dist_info$start_values(df$x)
    # set fixed params
    fixed_par <- NULL
    if (data_type == "cdf" &&
        dist_name %in% c("triang", "pert")) {
      if (df$y[1] == 0.0) {
        # set fixed param min
        fixed_par <- c(fixed_par, min = df$x[1])
        # remove var. param min
        par <- par[!(names(par) %in% "min")]
      }
      n <- length(df$x)
      if (df$y[n] == 1.0) {
        # set fixed param max
        fixed_par <- c(fixed_par, max = df$x[n])
        # remove var. param max
        par <- par[!(names(par) %in% "max")]
      }
    }
    # do the fit
    fit <- optim(
      par     = par,
      fn      = fit_func,
      gr      = NULL,
      df, dist_func, fixed_par,
      method  = "L-BFGS-B",
      control = list(maxit = 1e4),
      lower   = dist_info$lower,
      upper   = dist_info$upper
    )
    # compute mean neg. loglike, aic, bic, extbic, and add to fit
    if (private$data_type == "cdf") {
      # log likelihood for least-square regression
      the_args <- c(list(q = df$x), as.list(fit$par), as.list(fixed_par))
      pred_y <- do.call(dist_info$cdf_func, args = the_args)
      res <- (df$y - pred_y)^2
      p <- dnorm(res, sd = sd(res))
      mean_neg_loglike <- -mean(log(p))
    } else {
      # pdf is fitted using maximum-likelihood method, but as risk function
      mean_neg_loglike <- fit$value
    }
    n <- length(df$x)
    p <- length(fit$par)
    fit$mean_neg_loglike <- mean_neg_loglike
    fit$aic              <- mean_neg_loglike + p/n
    fit$bic              <- mean_neg_loglike + p * log(n)/(2*n)
    fit$extbic           <- mean_neg_loglike + p * log(n)/n
    # add fixed params to param to fitted param list fit$par
    fit$par <- c(fit$par, fixed_par)
    # return fit object
    fit
  }
)

rriskDistributionsClass$set("private",
  "fit_pdf", function(par, df, pdf_func, fixed_par)
  {
    # compute risk
    the_args <- c(list(x = df$x), as.list(par), as.list(fixed_par))
    p <- do.call(pdf_func, args = the_args)
    risk <- -mean(log(p))
    if (is.infinite(abs(risk)) || is.nan(risk)) risk <- 1e9
    risk
  }
)

rriskDistributionsClass$set("private",
  "fit_cdf", function(pars, df, cdf_func, fixed_par)
  {
    # build the argument list for cdf function
    the_args <- c(list(q = df$x), as.list(pars), as.list(fixed_par))
    # compute mu through cdf function
    pred_y <- do.call(cdf_func, args = the_args)
    # compute risk
    risk <- mean((df$y - pred_y)^2)
    # check if risk is a number
    if (is.infinite(abs(risk)) || is.nan(risk)) risk <- 1e10
    # return risk
    risk
  }
)

# rriskDistributionsClass$set("private",
#   "fit_cdf", function(pars, df, cdf_func)
#   {
#     # get the value of the precision parameter
#     phi <- 1/exp(pars["log_dispersion"])
#     # remove log_dispersion from parameter list
#     pars <- pars[-which(names(pars) == "log_dispersion")]
#     # build the argument list for cdf function
#     the_args <- c(list(q = df$x), as.list(pars))
#     # compute mu through cdf function
#     mu <- do.call(cdf_func, args = the_args)
#     # compute risk as average negative log-likelihood of beta distribution
#     tmp1  <- mu*phi
#     tmp2  <- (1 - mu)*phi
#     log_p <- lgamma(phi)-lgamma(tmp1)-lgamma(tmp2)+(tmp1-1)*log(df$y)+(tmp2-1)*log(1-df$y)
#     risk  <- -mean(log_p)
#     # check if risk is a number
#     if (is.infinite(abs(risk)) || is.nan(risk)) risk <- 1e10
#     # return risk
#     risk
#   }
# )

rriskDistributionsClass$set("private",
  "check_input_for_fitting", function(dist_name)
  {
    result <- list(is_ok = FALSE,
                   error_message = "")
    
    if (missing(dist_name)) {
      result$error_message <- "No distribution name given."
    } else if (!(dist_name %in% self$get_applicable_dists())) {
      result$error <- "distribution name unknown"
    } else if (is.null(private$provided_data)) {
      result$error_message <- "No data available"
    } else {
      result$is_ok <- TRUE
    }
    
    result
  }
)
#---END: private methods for density distribution fitting-----------------------