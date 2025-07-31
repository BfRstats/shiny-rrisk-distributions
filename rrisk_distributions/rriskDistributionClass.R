source("rrisk-distributions-core/rrisk-distributions-dist-fit-info.R")
source("rrisk-distributions-core/rrisk_distributions.R")
source("rrisk-distributions_utility_functions/preset_args_of_function.R")
source("rrisk-distributions_utility_functions/push_to_end.R")

rriskDistributionsClass <- R6::R6Class(
  classname = "rriskDistributionsClass",
  private = list(
    # objects for user data
    provided_data     = c(),
    provided_data_cdf = c(),
    data_type         = "pdf",
    fit_results       = list(),
    info_list         = list(),
    author_list       = list(),
    # list with available distributions
    dist_info_list    = list(),
    # prototype function for shiny Rrisk R6 class; makes R6 responsive
    tickle_shiny      = function() NULL
  ),
  public = list(
    initialize = function() {
      # add the the current dist_info_list 
      # (from rrisk-distributions-dist-fit-info.R)
      private$dist_info_list <- dist_info_list
      # prevent debug-mode when a stop is found
      options(error = NULL)
    }
    #finalize = function() {}
  )
)

# source and add methods
source("rrisk-distributions-methods/set-get-check-input-data.R")
source("rrisk-distributions-methods/fit-data-methods.R")
source("rrisk-distributions-methods/plot-histogram-and-ecdf.R")
source("rrisk-distributions-methods/add_get_change_remove_author_methods.R")
source("rrisk-distributions-methods/save_open_clear_model_methods.R")
source("rrisk-distributions-methods/set-get-info.R")
source("rrisk-distributions-methods/get-distribution-names.R")

source("rrisk-distributions-methods/atomic_check_methods.R")
