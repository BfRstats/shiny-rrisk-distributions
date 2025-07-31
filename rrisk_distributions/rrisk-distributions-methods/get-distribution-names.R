#---BEGIN: set functions--------------------------------------------------------
rriskDistributionsClass$set("public",
  "get_dist_names", function(applicable_only = FALSE)
  {
    if (applicable_only)
      names(private$fit_results)
    else
      names(private$dist_info_list)
  }
)

rriskDistributionsClass$set("public",
  "get_applicable_dists", function()
  {
    # check if data is there
    if (is.null(private$provided_data)) return(NULL)

    dist_names <- NULL
    
    if (is.numeric(private$provided_data)) {
      # data is numeric
      if (is.integer(private$provided_data)) {
        # data are discrete
      } else {
        # data is continuous
        # get all continuous distributions
        #dist_names <- NULL
        min_x      <- min(private$provided_data)
        max_x      <- max(private$provided_data)
        for (dist_name in names(private$dist_info_list)) {
          this_dist <- private$dist_info_list[[dist_name]]
          if (this_dist$type == "continuous" &&
              min_x >= this_dist$range["min"] &&
              max_x <= this_dist$range["max"]) {
            dist_names <- c(dist_names, dist_name)
          }
        }
      }
    } #else {
      # data is categorical
    #}
    
    dist_names
  }
)
#---BEGIN: set methods----------------------------------------------------------